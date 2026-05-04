use super::emit_ctx::{EmitCtx, is_copy_type, should_borrow_param};
use super::expr::{
    aver_name_to_rust, classify_body_expr_plan_for_rust, classify_body_plan_for_rust,
    classify_dispatch_plan_for_rust, classify_thin_fn_def_for_rust, clone_arg,
    emit_body_plan_for_rust, emit_dispatch_table_match, emit_expr, emit_stmt,
};
use super::types::type_annotation_to_rust;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::ir::{BodyExprPlan, CallPlan, LeafOp, thin_kind_is_parent_thin_candidate};
use crate::types::{Type, parse_type_str};
/// Top-level Aver items → Rust items (structs, enums, functions, tests).
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

fn visibility_prefix(public: bool) -> &'static str {
    if public { "pub " } else { "" }
}

fn indent_block(block: &str, levels: usize) -> String {
    let indent = "    ".repeat(levels);
    block
        .lines()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("{indent}{line}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn guest_args_param(fd: &FnDef) -> Option<String> {
    fd.params.iter().find_map(|(name, type_ann)| {
        (name == "guestArgs" && parse_type_str(type_ann) == Type::List(Box::new(Type::Str)))
            .then(|| aver_name_to_rust(name))
    })
}

fn self_host_runtime_state(fd: &FnDef) -> Option<(String, String)> {
    let prog = fd
        .params
        .iter()
        .find_map(|(name, _)| (name == "prog").then(|| aver_name_to_rust(name)));
    let module_fns = fd
        .params
        .iter()
        .find_map(|(name, _)| (name == "moduleFns").then(|| aver_name_to_rust(name)));
    match (prog, module_fns) {
        (Some(prog), Some(module_fns)) => Some((prog, module_fns)),
        _ => None,
    }
}

/// Emit a Rust struct or enum from an Aver TypeDef.
#[allow(dead_code)]
pub fn emit_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_with_visibility(td, false, ctx)
}

pub fn emit_public_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_with_visibility(td, true, ctx)
}

fn emit_type_def_with_visibility(td: &TypeDef, public: bool, ctx: &CodegenContext) -> String {
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants, public, ctx),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields, public, ctx),
    }
}

use crate::codegen::common::type_def_name;

fn find_type_def<'a>(name: &str, ctx: &'a CodegenContext) -> Option<&'a TypeDef> {
    ctx.type_defs
        .iter()
        .find(|td| type_def_name(td) == name)
        .or_else(|| {
            ctx.modules
                .iter()
                .flat_map(|module| module.type_defs.iter())
                .find(|td| type_def_name(td) == name)
        })
}

fn rust_hash_eq_safe_type(
    ty: &crate::types::Type,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> bool {
    use crate::types::Type;

    match ty {
        Type::Int | Type::Bool | Type::Unit | Type::Str => true,
        Type::Float => false,
        Type::Result(ok, err) => {
            rust_hash_eq_safe_type(ok, ctx, visiting) && rust_hash_eq_safe_type(err, ctx, visiting)
        }
        Type::Option(inner) => rust_hash_eq_safe_type(inner, ctx, visiting),
        Type::List(_) | Type::Vector(_) => false,
        Type::Tuple(items) => items
            .iter()
            .all(|item| rust_hash_eq_safe_type(item, ctx, visiting)),
        Type::Map(_, _) | Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => false,
        Type::Named(name) => rust_hash_eq_safe_named(name, ctx, visiting),
    }
}

fn rust_hash_eq_safe_named(
    name: &str,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> bool {
    if !visiting.insert(name.to_string()) {
        return true;
    }

    let safe = find_type_def(name, ctx).is_some_and(|td| match td {
        TypeDef::Sum { variants, .. } => variants.iter().all(|variant| {
            variant.fields.iter().all(|field_ty| {
                let parsed = crate::types::parse_type_str(field_ty);
                rust_hash_eq_safe_type(&parsed, ctx, visiting)
            })
        }),
        TypeDef::Product { fields, .. } => fields.iter().all(|(_, field_ty)| {
            let parsed = crate::types::parse_type_str(field_ty);
            rust_hash_eq_safe_type(&parsed, ctx, visiting)
        }),
    });

    visiting.remove(name);
    safe
}

fn type_can_derive_hash_eq(td: &TypeDef, ctx: &CodegenContext) -> bool {
    let mut visiting = HashSet::new();
    rust_hash_eq_safe_named(type_def_name(td), ctx, &mut visiting)
}

fn fn_supports_rust_memo(fd: &FnDef, ctx: &CodegenContext) -> bool {
    ctx.fn_sigs.get(&fd.name).is_some_and(|(params, _, _)| {
        params.iter().all(|param| {
            let mut visiting = HashSet::new();
            rust_hash_eq_safe_type(param, ctx, &mut visiting)
        })
    })
}

fn memo_key_component_expr(name: &str, ty: &crate::types::Type) -> String {
    if is_copy_type(ty) {
        name.to_string()
    } else {
        format!("{}.clone()", name)
    }
}

fn emit_sum_type(
    name: &str,
    variants: &[TypeVariant],
    public: bool,
    ctx: &CodegenContext,
) -> String {
    let mut out = String::new();
    let visibility = visibility_prefix(public);
    let derives = if type_can_derive_hash_eq(
        &TypeDef::Sum {
            name: name.to_string(),
            variants: variants.to_vec(),
            line: 0,
        },
        ctx,
    ) {
        "#[derive(Clone, Debug, PartialEq, Eq, Hash)]"
    } else {
        "#[derive(Clone, Debug, PartialEq)]"
    };
    writeln!(out, "{}", derives).unwrap();
    writeln!(out, "{}enum {} {{", visibility, name).unwrap();
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
                        // Recursive field: Rc<T> instead of Box<T> — clone is O(1) refcount bump
                        format!("std::sync::Arc<{}>", rust_ty)
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
            if v.fields.len() == 1 {
                // Single field: direct format without vec![].join() allocation
                writeln!(
                    out,
                    "            {}::{}({}) => format!(\"{}({{}})\", {}),",
                    name, v.name, bindings[0], v.name, display_parts[0]
                )
                .unwrap();
            } else {
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

fn emit_product_type(
    name: &str,
    fields: &[(String, String)],
    public: bool,
    ctx: &CodegenContext,
) -> String {
    let mut out = String::new();
    let visibility = visibility_prefix(public);
    let derives = if type_can_derive_hash_eq(
        &TypeDef::Product {
            name: name.to_string(),
            fields: fields.to_vec(),
            line: 0,
        },
        ctx,
    ) {
        "#[derive(Clone, Debug, PartialEq, Eq, Hash)]"
    } else {
        "#[derive(Clone, Debug, PartialEq)]"
    };
    writeln!(out, "{}", derives).unwrap();
    writeln!(out, "{}struct {} {{", visibility, name).unwrap();
    for (field_name, field_type) in fields {
        writeln!(
            out,
            "    {}{}: {},",
            visibility,
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
    if fields.len() == 1 {
        // Single field: direct format without vec![].join() allocation
        writeln!(out, "        format!(\"{}({{}})\", {})", name, parts[0]).unwrap();
    } else {
        writeln!(
            out,
            "        format!(\"{}({{}})\", vec![{}].join(\", \"))",
            name,
            parts.join(", ")
        )
        .unwrap();
    }
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    fn aver_display_inner(&self) -> String {{ self.aver_display() }}"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
}

/// Collect local_types from function signature or annotations.
fn collect_fn_local_types(fd: &FnDef, ctx: &CodegenContext) -> HashMap<String, Type> {
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
    local_types
}

/// Build an EmitCtx for a function from its parameter types in fn_sigs.
/// Uses borrow-by-default: non-Copy, non-Str params are tracked as borrowed.
fn build_fn_ectx(fd: &FnDef, ctx: &CodegenContext) -> EmitCtx {
    EmitCtx::for_fn(collect_fn_local_types(fd, ctx))
}

/// Build an EmitCtx for a function WITHOUT borrow-by-default.
/// Used for TCO and memo functions where params need to be owned/mutable.
fn build_fn_ectx_no_borrow(fd: &FnDef, ctx: &CodegenContext) -> EmitCtx {
    EmitCtx::for_fn_no_borrow(collect_fn_local_types(fd, ctx))
}

/// Emit a Rust function from an Aver FnDef.
#[allow(dead_code)]
pub fn emit_fn_def(fd: &FnDef, is_memo: bool, ctx: &CodegenContext) -> String {
    emit_fn_def_with_visibility(fd, is_memo, ctx, false)
}

pub fn emit_public_fn_def(fd: &FnDef, is_memo: bool, ctx: &CodegenContext) -> String {
    emit_fn_def_with_visibility(fd, is_memo, ctx, true)
}

fn emit_fn_def_with_visibility(
    fd: &FnDef,
    is_memo: bool,
    ctx: &CodegenContext,
    public: bool,
) -> String {
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
    let visibility = visibility_prefix(public);

    let use_memo = is_memo && fn_supports_rust_memo(fd, ctx);
    let is_guest_entry = ctx.guest_entry.as_deref() == Some(fd.name.as_str());

    // TCO functions need owned/mutable params, no borrow-by-default.
    // Memo functions always use borrow-by-default (memo wrapper takes &T).
    // Normal functions use borrow-by-default for non-Copy, non-Str params.
    let ectx = if has_tco && !use_memo {
        build_fn_ectx_no_borrow(fd, ctx)
    } else {
        build_fn_ectx(fd, ctx)
    };

    let guest_args_name = if is_guest_entry {
        guest_args_param(fd)
    } else {
        None
    };
    let self_host_state = if is_guest_entry && ctx.emit_self_host_support {
        self_host_runtime_state(fd)
    } else {
        None
    };
    let optimized_thin_plan = classify_thin_fn_def_for_rust(fd, ctx, &ectx);

    if fd.effects.is_empty()
        && optimized_thin_plan
            .as_ref()
            .is_some_and(|plan| thin_kind_is_parent_thin_candidate(plan.kind))
    {
        lines.push("#[inline(always)]".to_string());
    }

    if is_guest_entry && (ctx.emit_replay_runtime || self_host_state.is_some()) {
        lines.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        let mut wrapped_body = emit_fn_body(&fd.body, ctx, &ectx);
        if let Some((prog_name, module_fns_name)) = &self_host_state {
            wrapped_body = format!(
                "crate::self_host_support::with_program_fn_store({}.fns.clone(), {}.clone(), || {{\n{}\n}})",
                prog_name,
                module_fns_name,
                indent_block(&wrapped_body, 1)
            );
        }
        if ctx.emit_replay_runtime {
            match &guest_args_name {
                Some(guest_args) => {
                    // If the guest_args param is borrowed (&T), it's already a reference
                    let is_borrowed = ectx.is_borrowed_param(
                        &fd.params
                            .iter()
                            .find(|(n, _)| aver_name_to_rust(n) == *guest_args)
                            .map(|(n, _)| n.clone())
                            .unwrap_or_default(),
                    );
                    let ref_prefix = if is_borrowed { "" } else { "&" };
                    lines.push(format!(
                        "    let __replay_input = aver_replay::ReplayValue::to_replay_json({}{});",
                        ref_prefix, guest_args
                    ));
                    if fd.return_type.starts_with("Result<") {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_args_result({:?}, __replay_input, {}.clone(), || {{",
                            fd.name, guest_args
                        ));
                    } else {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_args({:?}, __replay_input, {}.clone(), || {{",
                            fd.name, guest_args
                        ));
                    }
                }
                None => {
                    let input_args = fd
                        .params
                        .iter()
                        .map(|(name, _)| {
                            let is_borrowed = ectx.is_borrowed_param(name);
                            let ref_prefix = if is_borrowed { "" } else { "&" };
                            format!(
                                "aver_replay::ReplayValue::to_replay_json({}{})",
                                ref_prefix,
                                aver_name_to_rust(name)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    lines.push(format!(
                        "    let __replay_input = aver_replay::entry_input(vec![{}]);",
                        input_args
                    ));
                    if fd.return_type.starts_with("Result<") {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_result({:?}, __replay_input, || {{",
                            fd.name
                        ));
                    } else {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope({:?}, __replay_input, || {{",
                            fd.name
                        ));
                    }
                }
            }
            lines.push(indent_block(&wrapped_body, 2));
            lines.push("    })".to_string());
        } else {
            lines.push(indent_block(&wrapped_body, 1));
        }
        lines.push("}".to_string());
        return lines.join("\n");
    }

    if use_memo {
        lines.push(emit_memo_fn(
            fd, &fn_name, &params, &ret_type, ctx, &ectx, visibility,
        ));
    } else if has_tco {
        lines.push(emit_tco_fn(fd, &fn_name, &ret_type, ctx, &ectx, visibility));
    } else {
        lines.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        lines.push(emit_fn_body(&fd.body, ctx, &ectx));
        lines.push("}".to_string());
    }

    lines.join("\n")
}

fn emit_fn_params(params: &[(String, String)], mutable: bool) -> String {
    emit_fn_params_with_rc(params, mutable, &HashSet::new())
}

/// Emit function params for self-TCO: non-Rc params are `mut`, Rc params are not
/// (they'll be shadowed by `let x = Rc::new(x)` before the loop).
fn emit_fn_params_tco(params: &[(String, String)], rc_indices: &HashSet<usize>) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            let rust_type = type_annotation_to_rust(type_ann);
            let rust_name = aver_name_to_rust(name);
            if rc_indices.contains(&i) {
                // Rc-wrapped: no `mut` needed (will be shadowed by Rc::new)
                format!("{}: {}", rust_name, rust_type)
            } else {
                format!("mut {}: {}", rust_name, rust_type)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_fn_params_with_rc(
    params: &[(String, String)],
    mutable: bool,
    rc_indices: &HashSet<usize>,
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            let rust_type = type_annotation_to_rust(type_ann);
            let rust_name = aver_name_to_rust(name);
            if rc_indices.contains(&i) {
                // Borrowed pass-through param: &T instead of owned T
                format!("{}: &{}", rust_name, rust_type)
            } else if mutable {
                format!("mut {}: {}", rust_name, rust_type)
            } else {
                // Borrow-by-default: non-Copy, non-Str params are `&T`
                let ty = parse_type_str(type_ann);
                if should_borrow_param(&ty) {
                    format!("{}: &{}", rust_name, rust_type)
                } else {
                    format!("{}: {}", rust_name, rust_type)
                }
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_fn_body(body: &FnBody, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    if let Some(plan) = classify_body_plan_for_rust(body, ctx, ectx) {
        return format!(
            "    crate::cancel_checkpoint();\n    {}",
            emit_body_plan_for_rust(&plan, ctx, ectx)
        );
    }

    let stmts = body.stmts();
    let mut lines = Vec::new();
    lines.push("    crate::cancel_checkpoint();".to_string());
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        match stmt {
            Stmt::Binding(name, type_ann, _) => {
                lines.push(format!("    {}", emit_stmt(stmt, ctx, ectx)));
                let _ = (name, type_ann);
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!("    {}", emit_expr(&expr.node, ctx, ectx)));
                } else {
                    lines.push(format!("    {};", emit_expr(&expr.node, ctx, ectx)));
                }
            }
        }
    }
    lines.join("\n")
}

/// Recursively check if an expression contains the `?` (ErrorProp) operator.
fn expr_uses_error_prop(expr: &Expr) -> bool {
    match expr {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(f, args) => {
            expr_uses_error_prop(&f.node) || args.iter().any(|a| expr_uses_error_prop(&a.node))
        }
        Expr::BinOp(_, l, r) => expr_uses_error_prop(&l.node) || expr_uses_error_prop(&r.node),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(&subject.node)
                || arms.iter().any(|a| expr_uses_error_prop(&a.body.node))
        }
        Expr::List(es) => es.iter().any(|e| expr_uses_error_prop(&e.node)),
        Expr::Tuple(es) | Expr::IndependentProduct(es, _) => {
            es.iter().any(|e| expr_uses_error_prop(&e.node))
        }
        Expr::Attr(e, _) => expr_uses_error_prop(&e.node),
        Expr::Constructor(_, Some(e)) => expr_uses_error_prop(&e.node),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_uses_error_prop(&e.node),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_uses_error_prop(&e.node))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(&base.node)
                || updates.iter().any(|(_, e)| expr_uses_error_prop(&e.node))
        }
        _ => false,
    }
}

pub(super) fn body_has_self_tailcall(body: &FnBody, fn_name: &str) -> bool {
    body.stmts().iter().any(|s| match s {
        Stmt::Expr(e) => expr_has_self_tailcall(&e.node, fn_name),
        Stmt::Binding(_, _, e) => expr_has_self_tailcall(&e.node, fn_name),
    })
}

fn expr_has_self_tailcall(expr: &Expr, fn_name: &str) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData {
                target, args: _, ..
            } = boxed.as_ref();
            target == fn_name
        }
        Expr::Match { arms, .. } => arms
            .iter()
            .any(|arm| expr_has_self_tailcall(&arm.body.node, fn_name)),
        _ => false,
    }
}

/// Is this Aver type expensive to clone (i.e. not Copy and not AverStr which is Rc<str>)?
fn is_expensive_clone_type(ty: &crate::types::Type) -> bool {
    use crate::types::Type;
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit => false, // Copy
        Type::Str => false, // AverStr is Rc<str>, clone is O(1)
        _ => true,
    }
}

/// For a group of mutually-recursive functions (or a single self-recursive fn),
/// find param indices that are "pass-through" — never rebound in tail calls.
/// These can safely be passed as `&T` borrows to avoid deep cloning.
fn compute_rc_params(group_fns: &[&FnDef], _ctx: &CodegenContext) -> HashSet<usize> {
    if group_fns.is_empty() {
        return HashSet::new();
    }

    // Try index-based first (works when all fns have same arity)
    let arity = group_fns[0].params.len();
    if group_fns.iter().all(|fd| fd.params.len() == arity) {
        return compute_rc_params_by_index(group_fns);
    }

    // Different arities: use name+type-based detection.
    // Find params (name, type) that appear in ALL functions and are pass-through.
    compute_rc_params_by_name(group_fns)
}

/// Index-based Rc detection: all fns have same arity, check same position.
fn compute_rc_params_by_index(group_fns: &[&FnDef]) -> HashSet<usize> {
    let arity = group_fns[0].params.len();
    let member_names: HashSet<&str> = group_fns.iter().map(|fd| fd.name.as_str()).collect();

    let mut candidates: HashSet<usize> = (0..arity)
        .filter(|&i| {
            let type_ann = &group_fns[0].params[i].1;
            let ty = crate::types::parse_type_str(type_ann);
            group_fns.iter().all(|fd| fd.params[i].1 == *type_ann) && is_expensive_clone_type(&ty)
        })
        .collect();

    if candidates.is_empty() {
        return candidates;
    }

    for fd in group_fns {
        check_tailcalls_for_rc(&fd.body, &member_names, &fd.params, &mut candidates);
        if candidates.is_empty() {
            break;
        }
    }
    candidates
}

/// Name+type-based Rc detection for groups with varying arities.
/// Finds params that share the same name AND type across all functions,
/// and are always passed through unchanged in tail calls.
/// Returns indices into the FIRST function's param list.
fn compute_rc_params_by_name(group_fns: &[&FnDef]) -> HashSet<usize> {
    // Build a map: fn_name → {param_name → (index, type)}
    let fn_param_map: HashMap<&str, HashMap<&str, (usize, &str)>> = group_fns
        .iter()
        .map(|fd| {
            let params: HashMap<&str, (usize, &str)> = fd
                .params
                .iter()
                .enumerate()
                .map(|(i, (name, ty))| (name.as_str(), (i, ty.as_str())))
                .collect();
            (fd.name.as_str(), params)
        })
        .collect();

    let member_names: HashSet<&str> = group_fns.iter().map(|fd| fd.name.as_str()).collect();

    // Find param names that exist in ALL functions with same type and expensive to clone
    let mut shared_params: Vec<(&str, &str)> = Vec::new(); // (name, type)
    if let Some(first) = group_fns.first() {
        for (name, ty) in &first.params {
            let parsed = crate::types::parse_type_str(ty);
            if !is_expensive_clone_type(&parsed) {
                continue;
            }
            // Check if ALL other fns have a param with same name and type
            let all_have_it = group_fns
                .iter()
                .all(|fd| fd.params.iter().any(|(n, t)| n == name && t == ty));
            if all_have_it {
                shared_params.push((name.as_str(), ty.as_str()));
            }
        }
    }

    if shared_params.is_empty() {
        return HashSet::new();
    }

    // For each shared param, check pass-through in ALL tail calls across ALL fns
    let valid_params: HashSet<&str> = shared_params
        .iter()
        .filter(|(param_name, _)| {
            group_fns.iter().all(|fd| {
                check_param_passthrough_by_name(&fd.body, &member_names, param_name, &fn_param_map)
            })
        })
        .map(|(name, _)| *name)
        .collect();

    // Convert back to indices into the first function's param list
    if let Some(first) = group_fns.first() {
        first
            .params
            .iter()
            .enumerate()
            .filter(|(_, (name, _))| valid_params.contains(name.as_str()))
            .map(|(i, _)| i)
            .collect()
    } else {
        HashSet::new()
    }
}

/// Check that every TailCall in `body` to a group member passes `param_name`
/// at the correct position in the TARGET function's param list.
fn check_param_passthrough_by_name(
    body: &FnBody,
    member_names: &HashSet<&str>,
    param_name: &str,
    fn_param_map: &HashMap<&str, HashMap<&str, (usize, &str)>>,
) -> bool {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => {
                if !check_expr_passthrough_by_name(&e.node, member_names, param_name, fn_param_map)
                {
                    return false;
                }
            }
        }
    }
    true
}

fn check_expr_passthrough_by_name(
    expr: &Expr,
    member_names: &HashSet<&str>,
    param_name: &str,
    fn_param_map: &HashMap<&str, HashMap<&str, (usize, &str)>>,
) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if !member_names.contains(target.as_str()) {
                return true; // call to non-member, irrelevant
            }
            // Find the index of param_name in the TARGET function
            if let Some(target_params) = fn_param_map.get(target.as_str())
                && let Some(&(target_idx, _)) = target_params.get(param_name)
            {
                // arg at target_idx must be Ident(param_name) from the caller
                target_idx < args.len()
                    && matches!(&args[target_idx].node, Expr::Ident(name) if name == param_name)
            } else {
                false
            }
        }
        Expr::Match { arms, .. } => arms.iter().all(|arm| {
            check_expr_passthrough_by_name(&arm.body.node, member_names, param_name, fn_param_map)
        }),
        _ => true,
    }
}

/// Walk the AST and verify that every TailCall to a group member passes
/// param[i] as Ident(param_name[i]) for all candidate indices.
/// Removes candidates that fail the check.
fn check_tailcalls_for_rc(
    body: &FnBody,
    member_names: &HashSet<&str>,
    params: &[(String, String)],
    candidates: &mut HashSet<usize>,
) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => {
                check_expr_tailcalls_for_rc(&e.node, member_names, params, candidates);
            }
        }
    }
}

fn check_expr_tailcalls_for_rc(
    expr: &Expr,
    member_names: &HashSet<&str>,
    params: &[(String, String)],
    candidates: &mut HashSet<usize>,
) {
    if candidates.is_empty() {
        return;
    }
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if member_names.contains(target.as_str()) && args.len() == params.len() {
                // For each candidate index, check if arg[i] == Ident(param_name[i])
                let to_remove: Vec<usize> = candidates
                    .iter()
                    .copied()
                    .filter(
                        |&i| !matches!(&args[i].node, Expr::Ident(name) if *name == params[i].0),
                    )
                    .collect();
                for idx in to_remove {
                    candidates.remove(&idx);
                }
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                check_expr_tailcalls_for_rc(&arm.body.node, member_names, params, candidates);
            }
        }
        _ => {}
    }
}

/// Build a set of param names that should be borrowed (`&T`), given rc_indices.
fn rc_param_names(params: &[(String, String)], rc_indices: &HashSet<usize>) -> HashSet<String> {
    rc_indices
        .iter()
        .filter_map(|&i| params.get(i).map(|(name, _)| name.clone()))
        .collect()
}

#[derive(Debug)]
struct TcoInvariantHoist<'a> {
    ptr: usize,
    temp_name: String,
    expr: &'a Expr,
}

fn expr_ptr(expr: &Expr) -> usize {
    expr as *const Expr as usize
}

fn passthrough_param_names(
    params: &[(String, String)],
    passthrough_indices: &HashSet<usize>,
) -> HashSet<String> {
    passthrough_indices
        .iter()
        .filter_map(|&i| params.get(i).map(|(name, _)| name.clone()))
        .collect()
}

fn lookup_call_effects<'a>(name: &str, ctx: &'a CodegenContext) -> Option<&'a Vec<String>> {
    if let Some((_, _, effects)) = ctx.fn_sigs.get(name) {
        return Some(effects);
    }

    let bare = name.rsplit('.').next().unwrap_or(name);
    let suffix = format!(".{}", bare);
    let mut matches = ctx
        .fn_sigs
        .iter()
        .filter_map(|(candidate, (_, _, effects))| {
            (candidate == name || candidate == bare || candidate.ends_with(&suffix))
                .then_some(effects)
        })
        .collect::<Vec<_>>();
    if matches.len() == 1 {
        matches.pop()
    } else {
        None
    }
}

fn call_plan_is_effect_free(plan: &CallPlan, ctx: &CodegenContext) -> bool {
    match plan {
        CallPlan::Dynamic => false,
        CallPlan::Function(name) | CallPlan::Builtin(name) => {
            lookup_call_effects(name, ctx).is_some_and(|effects| effects.is_empty())
        }
        CallPlan::Wrapper(_) | CallPlan::NoneValue | CallPlan::TypeConstructor { .. } => true,
    }
}

fn expr_is_loop_invariant(
    expr: &Expr,
    stable_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> bool {
    match expr {
        Expr::Literal(_) => true,
        Expr::Ident(name) => stable_names.contains(name),
        Expr::Resolved { .. } | Expr::ErrorProp(_) | Expr::TailCall(_) => false,
        Expr::Attr(obj, _) => {
            crate::ir::expr_to_dotted_name(expr)
                .is_some_and(|dotted| dotted.chars().next().is_some_and(|c| c.is_uppercase()))
                || expr_is_loop_invariant(&obj.node, stable_names, ctx, ectx)
        }
        Expr::FnCall(_, args) => match classify_body_expr_plan_for_rust(expr, ctx, ectx) {
            BodyExprPlan::Leaf(_) => args
                .iter()
                .all(|arg| expr_is_loop_invariant(&arg.node, stable_names, ctx, ectx)),
            BodyExprPlan::Call { target, args } => {
                call_plan_is_effect_free(&target, ctx)
                    && args
                        .iter()
                        .all(|arg| expr_is_loop_invariant(&arg.node, stable_names, ctx, ectx))
            }
            BodyExprPlan::ForwardCall(plan) => {
                call_plan_is_effect_free(&plan.target, ctx)
                    && args
                        .iter()
                        .all(|arg| expr_is_loop_invariant(&arg.node, stable_names, ctx, ectx))
            }
            BodyExprPlan::Expr(_) => false,
        },
        Expr::BinOp(_, left, right) => {
            expr_is_loop_invariant(&left.node, stable_names, ctx, ectx)
                && expr_is_loop_invariant(&right.node, stable_names, ctx, ectx)
        }
        Expr::Match { subject, arms, .. } => {
            expr_is_loop_invariant(&subject.node, stable_names, ctx, ectx)
                && arms
                    .iter()
                    .all(|arm| expr_is_loop_invariant(&arm.body.node, stable_names, ctx, ectx))
        }
        Expr::Constructor(_, Some(inner)) => {
            expr_is_loop_invariant(&inner.node, stable_names, ctx, ectx)
        }
        Expr::Constructor(_, None) => true,
        Expr::InterpolatedStr(parts) => parts.iter().all(|part| match part {
            StrPart::Literal(_) => true,
            StrPart::Parsed(expr) => expr_is_loop_invariant(&expr.node, stable_names, ctx, ectx),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
            .iter()
            .all(|item| expr_is_loop_invariant(&item.node, stable_names, ctx, ectx)),
        Expr::MapLiteral(entries) => entries.iter().all(|(key, value)| {
            expr_is_loop_invariant(&key.node, stable_names, ctx, ectx)
                && expr_is_loop_invariant(&value.node, stable_names, ctx, ectx)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .all(|(_, value)| expr_is_loop_invariant(&value.node, stable_names, ctx, ectx)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_is_loop_invariant(&base.node, stable_names, ctx, ectx)
                && updates
                    .iter()
                    .all(|(_, value)| expr_is_loop_invariant(&value.node, stable_names, ctx, ectx))
        }
    }
}

/// `Buffer` (alias for the runtime `aver_rt::Buffer` type) is what every
/// `__buf_*` builder synthesizes. Buffers are mutable-owned scratch
/// storage that gets consumed exactly once — perfect candidate for the
/// "do not hoist" rule below.
fn type_is_owned_mut_buffer(ty: &Type) -> bool {
    matches!(ty, Type::Named(name) if name == "Buffer")
}

fn expr_is_hoistable_invariant(
    expr: &Spanned<Expr>,
    stable_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> bool {
    if !expr_is_loop_invariant(&expr.node, stable_names, ctx, ectx) {
        return false;
    }

    // Buffer-build sinks return an owned `aver_rt::Buffer` (≈ String).
    // Hoisting an owned-mutable buffer outside the loop is wrong: every
    // loop iteration *consumes* the buffer (it gets moved into a `__b`
    // local that grows with `push_str` and is then converted to
    // `AverStr`). After iter 0 the hoisted local has been moved away, so
    // iter 1 dereferences a moved value and rustc rejects it. Cloning
    // per-iter would defeat the hoist anyway.
    //
    // The check is type-driven (Step 1: TypeChecker stamps every
    // `Spanned<Expr>` with its inferred `Type`). Any expression that
    // produces a `Buffer` — `__buf_new(...)`, `__buf_push_*(...)`,
    // future buffer constructors — falls under the rule by virtue of
    // its return type, no string-name allowlist required.
    if let Some(ty) = expr.ty()
        && type_is_owned_mut_buffer(ty)
    {
        return false;
    }

    match classify_body_expr_plan_for_rust(&expr.node, ctx, ectx) {
        BodyExprPlan::Leaf(
            LeafOp::StaticRef(_) | LeafOp::NoneValue | LeafOp::VariantConstructor { .. },
        ) => false,
        BodyExprPlan::Leaf(_) => true,
        BodyExprPlan::Call { target, .. } => call_plan_is_effect_free(&target, ctx),
        BodyExprPlan::ForwardCall(plan) => call_plan_is_effect_free(&plan.target, ctx),
        BodyExprPlan::Expr(_) => false,
    }
}

fn collect_hoistable_invariant_subexprs<'a>(
    expr: &'a Spanned<Expr>,
    stable_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    hoists: &mut Vec<TcoInvariantHoist<'a>>,
    seen: &mut HashSet<usize>,
    next_idx: &mut usize,
) {
    if expr_is_hoistable_invariant(expr, stable_names, ctx, ectx) {
        let ptr = expr_ptr(&expr.node);
        if seen.insert(ptr) {
            hoists.push(TcoInvariantHoist {
                ptr,
                temp_name: format!("__aver_inv{}", *next_idx),
                expr: &expr.node,
            });
            *next_idx += 1;
        }
        return;
    }

    match &expr.node {
        Expr::Attr(obj, _) => collect_hoistable_invariant_subexprs(
            obj,
            stable_names,
            ctx,
            ectx,
            hoists,
            seen,
            next_idx,
        ),
        Expr::FnCall(fn_expr, args) => {
            collect_hoistable_invariant_subexprs(
                fn_expr,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for arg in args {
                collect_hoistable_invariant_subexprs(
                    arg,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_hoistable_invariant_subexprs(
                left,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            collect_hoistable_invariant_subexprs(
                right,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
        }
        Expr::Match { subject, arms, .. } => {
            collect_hoistable_invariant_subexprs(
                subject,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for arm in arms {
                collect_hoistable_invariant_subexprs(
                    &arm.body,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            collect_hoistable_invariant_subexprs(
                inner,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            )
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_hoistable_invariant_subexprs(
                        inner,
                        stable_names,
                        ctx,
                        ectx,
                        hoists,
                        seen,
                        next_idx,
                    );
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_hoistable_invariant_subexprs(
                    item,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_hoistable_invariant_subexprs(
                    key,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
                collect_hoistable_invariant_subexprs(
                    value,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_hoistable_invariant_subexprs(
                    value,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_hoistable_invariant_subexprs(
                base,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for (_, value) in updates {
                collect_hoistable_invariant_subexprs(
                    value,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None)
        | Expr::TailCall(_) => {}
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_self_tailcall_hoists_in_expr<'a>(
    expr: &'a Spanned<Expr>,
    self_name: &str,
    stable_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    hoists: &mut Vec<TcoInvariantHoist<'a>>,
    seen: &mut HashSet<usize>,
    next_idx: &mut usize,
) {
    match &expr.node {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if target == self_name {
                for arg in args {
                    collect_hoistable_invariant_subexprs(
                        arg,
                        stable_names,
                        ctx,
                        ectx,
                        hoists,
                        seen,
                        next_idx,
                    );
                }
            } else {
                for arg in args {
                    collect_self_tailcall_hoists_in_expr(
                        arg,
                        self_name,
                        stable_names,
                        ctx,
                        ectx,
                        hoists,
                        seen,
                        next_idx,
                    );
                }
            }
        }
        Expr::Match { subject, arms, .. } => {
            collect_self_tailcall_hoists_in_expr(
                subject,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for arm in arms {
                collect_self_tailcall_hoists_in_expr(
                    &arm.body,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::Attr(obj, _) => collect_self_tailcall_hoists_in_expr(
            obj,
            self_name,
            stable_names,
            ctx,
            ectx,
            hoists,
            seen,
            next_idx,
        ),
        Expr::FnCall(fn_expr, args) => {
            collect_self_tailcall_hoists_in_expr(
                fn_expr,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for arg in args {
                collect_self_tailcall_hoists_in_expr(
                    arg,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_self_tailcall_hoists_in_expr(
                left,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            collect_self_tailcall_hoists_in_expr(
                right,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            collect_self_tailcall_hoists_in_expr(
                inner,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            )
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_self_tailcall_hoists_in_expr(
                        inner,
                        self_name,
                        stable_names,
                        ctx,
                        ectx,
                        hoists,
                        seen,
                        next_idx,
                    );
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_self_tailcall_hoists_in_expr(
                    item,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_self_tailcall_hoists_in_expr(
                    key,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
                collect_self_tailcall_hoists_in_expr(
                    value,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_self_tailcall_hoists_in_expr(
                    value,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_self_tailcall_hoists_in_expr(
                base,
                self_name,
                stable_names,
                ctx,
                ectx,
                hoists,
                seen,
                next_idx,
            );
            for (_, value) in updates {
                collect_self_tailcall_hoists_in_expr(
                    value,
                    self_name,
                    stable_names,
                    ctx,
                    ectx,
                    hoists,
                    seen,
                    next_idx,
                );
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn collect_self_tailcall_invariant_hoists<'a>(
    body: &'a FnBody,
    self_name: &str,
    params: &[(String, String)],
    passthrough_indices: &HashSet<usize>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Vec<TcoInvariantHoist<'a>> {
    let stable_names = passthrough_param_names(params, passthrough_indices);
    let mut hoists = Vec::new();
    let mut seen = HashSet::new();
    let mut next_idx = 0usize;

    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => collect_self_tailcall_hoists_in_expr(
                expr,
                self_name,
                &stable_names,
                ctx,
                ectx,
                &mut hoists,
                &mut seen,
                &mut next_idx,
            ),
        }
    }

    hoists
}

fn rewrite_expr_with_hoists(expr: &Expr, hoisted_exprs: &HashMap<usize, String>) -> Expr {
    if let Some(name) = hoisted_exprs.get(&expr_ptr(expr)) {
        return Expr::Ident(name.clone());
    }

    fn rewrite_spanned(
        spanned: &Spanned<Expr>,
        hoisted_exprs: &HashMap<usize, String>,
    ) -> Spanned<Expr> {
        Spanned::new(
            rewrite_expr_with_hoists(&spanned.node, hoisted_exprs),
            spanned.line,
        )
    }

    match expr {
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Ident(name) => Expr::Ident(name.clone()),
        Expr::Resolved {
            slot,
            name,
            last_use,
        } => Expr::Resolved {
            slot: *slot,
            name: name.clone(),
            last_use: *last_use,
        },
        Expr::Attr(obj, field) => {
            Expr::Attr(Box::new(rewrite_spanned(obj, hoisted_exprs)), field.clone())
        }
        Expr::FnCall(fn_expr, args) => Expr::FnCall(
            Box::new(rewrite_spanned(fn_expr, hoisted_exprs)),
            args.iter()
                .map(|arg| rewrite_spanned(arg, hoisted_exprs))
                .collect(),
        ),
        Expr::BinOp(op, left, right) => Expr::BinOp(
            *op,
            Box::new(rewrite_spanned(left, hoisted_exprs)),
            Box::new(rewrite_spanned(right, hoisted_exprs)),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(rewrite_spanned(subject, hoisted_exprs)),
            arms: arms
                .iter()
                .map(|arm| MatchArm::new(
                    arm.pattern.clone(),
                    rewrite_spanned(&arm.body, hoisted_exprs),
                ))
                .collect(),
        },
        Expr::Constructor(name, inner) => Expr::Constructor(
            name.clone(),
            inner
                .as_ref()
                .map(|expr| Box::new(rewrite_spanned(expr, hoisted_exprs))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(rewrite_spanned(inner, hoisted_exprs))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(text) => StrPart::Literal(text.clone()),
                    StrPart::Parsed(expr) => {
                        StrPart::Parsed(Box::new(rewrite_spanned(expr, hoisted_exprs)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| rewrite_spanned(item, hoisted_exprs))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| rewrite_spanned(item, hoisted_exprs))
                .collect(),
        ),
        Expr::IndependentProduct(items, flag) => Expr::IndependentProduct(
            items
                .iter()
                .map(|item| rewrite_spanned(item, hoisted_exprs))
                .collect(),
            *flag,
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(key, value)| {
                    (
                        rewrite_spanned(key, hoisted_exprs),
                        rewrite_spanned(value, hoisted_exprs),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), rewrite_spanned(value, hoisted_exprs)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(rewrite_spanned(base, hoisted_exprs)),
            updates: updates
                .iter()
                .map(|(name, value)| (name.clone(), rewrite_spanned(value, hoisted_exprs)))
                .collect(),
        },
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            Expr::TailCall(Box::new(TailCallData::new(
                target.clone(),
                args.iter()
                    .map(|arg| rewrite_spanned(arg, hoisted_exprs))
                    .collect(),
            )))
        }
    }
}

/// Emit a function with TCO → loop rewrite.
fn emit_tco_fn(
    fd: &FnDef,
    fn_name: &str,
    ret_type: &str,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    visibility: &str,
) -> String {
    let passthrough_indices = compute_self_passthrough_params(fd);
    // Compute pass-through Rc params for self-TCO
    let rc_indices = compute_rc_params(&[fd], ctx);
    let rc_names = rc_param_names(&fd.params, &rc_indices);
    let ectx = if rc_names.is_empty() {
        ectx.clone()
    } else {
        ectx.with_rc_wrapped(rc_names)
    };
    let invariant_hoists = collect_self_tailcall_invariant_hoists(
        &fd.body,
        &fd.name,
        &fd.params,
        &passthrough_indices,
        ctx,
        &ectx,
    );
    let hoisted_exprs: HashMap<usize, String> = invariant_hoists
        .iter()
        .map(|hoist| (hoist.ptr, hoist.temp_name.clone()))
        .collect();

    // All params keep their original types in the public signature.
    // Non-Rc params are mutable (for rebinding in tail calls); Rc params don't need mut
    // since they'll be shadowed by Rc-wrapped `let` bindings before the loop.
    let params = emit_fn_params_tco(&fd.params, &rc_indices);
    let mut lines = Vec::new();
    lines.push(format!(
        "{}fn {}({}) -> {} {{",
        visibility, fn_name, params, ret_type
    ));

    // Wrap pass-through params in Rc before the loop (shadowing the original binding)
    for &i in &rc_indices {
        let (name, _) = &fd.params[i];
        let rust_name = aver_name_to_rust(name);
        lines.push(format!(
            "    let {} = std::sync::Arc::new({});",
            rust_name, rust_name
        ));
    }

    for hoist in &invariant_hoists {
        lines.push(format!(
            "    let {} = {};",
            hoist.temp_name,
            emit_expr(hoist.expr, ctx, &ectx)
        ));
    }

    lines.push("    loop {".to_string());

    // Emit body with TailCall → { reassign; continue }
    let body_code = emit_tco_body(
        &fd.body,
        &fd.name,
        &fd.params,
        ctx,
        &ectx,
        &rc_indices,
        &passthrough_indices,
        &hoisted_exprs,
    );
    lines.push(body_code);

    lines.push("    }".to_string());
    lines.push("}".to_string());
    lines.join("\n")
}

#[allow(clippy::too_many_arguments)]
fn emit_tco_body(
    body: &FnBody,
    self_name: &str,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
    passthrough_indices: &HashSet<usize>,
    hoisted_exprs: &HashMap<usize, String>,
) -> String {
    let stmts = body.stmts();
    let mut lines = Vec::new();
    lines.push("        crate::cancel_checkpoint();".to_string());
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        match stmt {
            Stmt::Binding(name, _, expr) => {
                lines.push(format!(
                    "        let {} = {};",
                    aver_name_to_rust(name),
                    emit_expr(&expr.node, ctx, ectx)
                ));
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!(
                        "        return {};",
                        emit_tco_expr(
                            &expr.node,
                            self_name,
                            params,
                            ctx,
                            ectx,
                            rc_indices,
                            passthrough_indices,
                            hoisted_exprs,
                        )
                    ));
                } else {
                    lines.push(format!("        {};", emit_expr(&expr.node, ctx, ectx)));
                }
            }
        }
    }
    lines.join("\n")
}

#[allow(clippy::too_many_arguments)]
fn try_emit_tco_bool_if_else(
    subj: &str,
    arms: &[MatchArm],
    self_name: &str,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
    passthrough_indices: &HashSet<usize>,
    hoisted_exprs: &HashMap<usize, String>,
) -> Option<String> {
    if arms.len() != 2 {
        return None;
    }
    let (true_body, false_body) = match (&arms[0].pattern, &arms[1].pattern) {
        (Pattern::Literal(Literal::Bool(true)), Pattern::Literal(Literal::Bool(false))) => {
            (&arms[0].body, &arms[1].body)
        }
        (Pattern::Literal(Literal::Bool(false)), Pattern::Literal(Literal::Bool(true))) => {
            (&arms[1].body, &arms[0].body)
        }
        _ => return None,
    };
    let t = emit_tco_expr(
        &true_body.node,
        self_name,
        params,
        ctx,
        ectx,
        rc_indices,
        passthrough_indices,
        hoisted_exprs,
    );
    let f = emit_tco_expr(
        &false_body.node,
        self_name,
        params,
        ctx,
        ectx,
        rc_indices,
        passthrough_indices,
        hoisted_exprs,
    );
    Some(format!("if {} {{ {} }} else {{ {} }}", subj, t, f))
}

#[allow(clippy::too_many_arguments)]
fn emit_tco_expr(
    expr: &Expr,
    self_name: &str,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
    passthrough_indices: &HashSet<usize>,
    hoisted_exprs: &HashMap<usize, String>,
) -> String {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if target != self_name || args.len() != params.len() {
                return emit_expr(expr, ctx, ectx);
            }

            // Self TCO — create temp vars, then reassign.
            // Skip Rc-wrapped params (they're pass-through, never rebound).
            let rewritten_args = args
                .iter()
                .map(|arg| rewrite_expr_with_hoists(&arg.node, hoisted_exprs))
                .collect::<Vec<_>>();

            // Clone/move decisions now come from last_use on Resolved nodes.
            // Just use the parent ectx directly.
            let arg_strs: Vec<String> = rewritten_args
                .iter()
                .map(|a| clone_arg(a, ctx, ectx))
                .collect();

            // Collect which params are being rebound (non-passthrough, non-identity).
            let rebound_param_names: HashSet<String> = params
                .iter()
                .enumerate()
                .filter(|(i, (name, _))| {
                    !passthrough_indices.contains(i)
                        && arg_strs
                            .get(*i)
                            .is_none_or(|a| *a != aver_name_to_rust(name))
                })
                .map(|(_, (name, _))| aver_name_to_rust(name))
                .collect();

            // Check if each arg references a rebound param (needs tmp to avoid
            // read-after-write). Simple heuristic: check if arg_str contains
            // any rebound param name as a substring.
            let needs_tmp: Vec<bool> = arg_strs
                .iter()
                .enumerate()
                .map(|(i, arg_str)| {
                    if passthrough_indices.contains(&i) {
                        return false;
                    }
                    let param_name = aver_name_to_rust(&params[i].0);
                    // Identity rebinding (x = x) → skip entirely
                    if *arg_str == param_name {
                        return false;
                    }
                    // If arg mentions any rebound param, must use tmp
                    rebound_param_names
                        .iter()
                        .any(|p| arg_str.contains(p.as_str()))
                })
                .collect();

            let any_tmp = needs_tmp.iter().any(|&t| t);
            let mut lines = Vec::new();
            lines.push("{".to_string());
            // Phase 1: tmp bindings for args that reference rebound params
            if any_tmp {
                for (i, arg_str) in arg_strs.iter().enumerate() {
                    if passthrough_indices.contains(&i) || !needs_tmp[i] {
                        continue;
                    }
                    lines.push(format!("            let __tmp{} = {};", i, arg_str));
                }
            }
            // Phase 2: assignments
            for (i, (name, _)) in params.iter().enumerate() {
                if passthrough_indices.contains(&i) {
                    continue;
                }
                let param_name = aver_name_to_rust(name);
                let arg_str = &arg_strs[i];
                if *arg_str == param_name {
                    continue; // identity rebinding — no-op
                }
                if needs_tmp[i] {
                    lines.push(format!("            {} = __tmp{};", param_name, i));
                } else {
                    lines.push(format!("            {} = {};", param_name, arg_str));
                }
            }
            lines.push("            continue;".to_string());
            lines.push("        }".to_string());
            lines.join("\n")
        }
        Expr::Match { subject, arms, .. } => {
            let subj = clone_arg(&subject.node, ctx, ectx);
            let dispatch_plan = classify_dispatch_plan_for_rust(arms, ctx, ectx);

            // Bool match → if/else in TCO context
            if let Some(code) = try_emit_tco_bool_if_else(
                &subj,
                arms,
                self_name,
                params,
                ctx,
                ectx,
                rc_indices,
                passthrough_indices,
                hoisted_exprs,
            ) {
                return code;
            }

            let needs_as_str = super::expr::has_string_literal_patterns(arms);
            if super::expr::has_list_patterns(arms) {
                return super::expr::emit_list_match(subj, arms, None, true, ctx, |arm| {
                    emit_tco_expr(
                        &arm.body.node,
                        self_name,
                        params,
                        ctx,
                        ectx,
                        rc_indices,
                        passthrough_indices,
                        hoisted_exprs,
                    )
                });
            }

            if let Some(crate::ir::MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
                return emit_dispatch_table_match(subj, arms, shape, |arm| {
                    emit_tco_expr(
                        &arm.body.node,
                        self_name,
                        params,
                        ctx,
                        ectx,
                        rc_indices,
                        passthrough_indices,
                        hoisted_exprs,
                    )
                });
            }

            let match_expr = if needs_as_str {
                format!("&*{}", subj)
            } else {
                subj
            };

            let mut arm_strs = Vec::new();
            for arm in arms {
                let pat = super::pattern::emit_pattern(&arm.pattern, needs_as_str, ctx);
                let body = emit_tco_expr(
                    &arm.body.node,
                    self_name,
                    params,
                    ctx,
                    ectx,
                    rc_indices,
                    passthrough_indices,
                    hoisted_exprs,
                );
                let mut rebinding_lines: Vec<String> = Vec::new();
                if let Pattern::Cons(head, tail) = &arm.pattern {
                    if head != "_" {
                        let h = aver_name_to_rust(head);
                        rebinding_lines.push(format!("let {} = {}.clone();", h, h));
                    }
                    let _ = tail;
                }
                if let Pattern::Constructor(name, bindings) = &arm.pattern {
                    // Ok/Err/Some bindings are moved, no clone. Only Box-wrapped fields need deref.
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
        _ => {
            // If this is a bare Rc-wrapped ident being returned, deref+clone to get T
            if let Expr::Ident(name) = expr
                && ectx.is_rc_wrapped(name)
            {
                let code = emit_expr(expr, ctx, ectx);
                return format!("(*{}).clone()", code);
            }
            emit_expr(expr, ctx, ectx)
        }
    }
}

fn compute_self_passthrough_params(fd: &FnDef) -> HashSet<usize> {
    let mut candidates: HashSet<usize> = (0..fd.params.len()).collect();
    let member_names = HashSet::from([fd.name.as_str()]);
    check_tailcalls_for_rc(&fd.body, &member_names, &fd.params, &mut candidates);
    candidates
}

// --- Mutual TCO (trampoline) support ---

/// Find groups of mutually tail-calling functions (SCCs of size > 1).
/// Returns indices into `fn_defs`.
pub fn find_mutual_tco_groups(fn_defs: &[&FnDef]) -> Vec<Vec<usize>> {
    let name_to_idx: HashMap<&str, usize> = fn_defs
        .iter()
        .enumerate()
        .map(|(i, fd)| (fd.name.as_str(), i))
        .collect();
    crate::call_graph::tailcall_scc_components(fn_defs)
        .into_iter()
        .map(|group| {
            let mut indices: Vec<usize> = group
                .iter()
                .filter_map(|fd| name_to_idx.get(fd.name.as_str()).copied())
                .collect();
            indices.sort();
            indices
        })
        .collect()
}

/// Convert an Aver function name to a PascalCase enum variant name.
fn fn_name_to_variant(name: &str) -> String {
    let rust_name = aver_name_to_rust(name);
    let mut chars = rust_name.chars();
    match chars.next() {
        Some(c) => {
            let upper: String = c.to_uppercase().collect();
            format!("{}{}", upper, chars.as_str())
        }
        None => rust_name,
    }
}

/// Emit a mutual TCO block: enum + trampoline dispatch loop + thin wrapper functions.
pub fn emit_mutual_tco_block(
    group_id: usize,
    group_fns: &[&FnDef],
    ctx: &CodegenContext,
    visibility: &str,
) -> String {
    let enum_name = format!("__MutualTco{}", group_id);
    let trampoline_name = format!("__mutual_tco_trampoline_{}", group_id);
    let ret_type = if group_fns[0].return_type.is_empty() {
        "()".to_string()
    } else {
        type_annotation_to_rust(&group_fns[0].return_type)
    };

    let member_names: HashSet<String> = group_fns.iter().map(|fd| fd.name.clone()).collect();
    let rc_indices = compute_rc_params(group_fns, ctx);
    let rc_names = if !group_fns.is_empty() {
        rc_param_names(&group_fns[0].params, &rc_indices)
    } else {
        HashSet::new()
    };

    let mut sections = Vec::new();

    // 1. Enum definition — exclude borrowed params (they're shared across iterations)
    let mut enum_lines = Vec::new();
    enum_lines.push("#[allow(non_camel_case_types)]".to_string());
    enum_lines.push(format!("enum {} {{", enum_name));
    for fd in group_fns {
        let variant = fn_name_to_variant(&fd.name);
        let param_types: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(_, ty)| type_annotation_to_rust(ty))
            .collect();
        if param_types.is_empty() {
            enum_lines.push(format!("    {},", variant));
        } else {
            enum_lines.push(format!("    {}({}),", variant, param_types.join(", ")));
        }
    }
    enum_lines.push("}".to_string());
    sections.push(enum_lines.join("\n"));

    // 2. Trampoline function — borrowed params are extra `&T` parameters
    let mut tramp_lines = Vec::new();

    // Build the borrowed extra params for the trampoline signature (&T)
    // Use first fn that has them (all fns have same name+type for borrowed params)
    let rc_extra_params: String = if !rc_names.is_empty() && !group_fns.is_empty() {
        let parts: Vec<String> = group_fns[0]
            .params
            .iter()
            .filter(|(name, _)| rc_names.contains(name))
            .map(|(name, ty)| {
                format!(
                    "{}: &{}",
                    aver_name_to_rust(name),
                    type_annotation_to_rust(ty)
                )
            })
            .collect();
        if parts.is_empty() {
            String::new()
        } else {
            format!(", {}", parts.join(", "))
        }
    } else {
        String::new()
    };

    tramp_lines.push(format!(
        "fn {}(mut __state: {}{}) -> {} {{",
        trampoline_name, enum_name, rc_extra_params, ret_type
    ));
    tramp_lines.push("    loop {".to_string());
    tramp_lines.push("        __state = match __state {".to_string());

    for fd in group_fns {
        let variant = fn_name_to_variant(&fd.name);
        let param_bindings: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, _)| format!("mut {}", aver_name_to_rust(name)))
            .collect();
        let binding = if param_bindings.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!("{}::{}({})", enum_name, variant, param_bindings.join(", "))
        };
        tramp_lines.push(format!("            {} => {{", binding));

        // Trampoline params are `mut T`, no borrow-by-default
        let ectx = build_fn_ectx_no_borrow(fd, ctx);
        let ectx = if rc_names.is_empty() {
            ectx
        } else {
            ectx.with_rc_wrapped(rc_names.clone())
        };
        let body_code =
            emit_trampoline_arm_body(fd, &enum_name, &member_names, ctx, &ectx, &rc_indices);
        tramp_lines.push(body_code);

        tramp_lines.push("            }".to_string());
    }

    tramp_lines.push("        };".to_string());
    tramp_lines.push("    }".to_string());
    tramp_lines.push("}".to_string());
    sections.push(tramp_lines.join("\n"));

    // 3. Wrapper functions — accept `&T` (borrow-by-default), clone into enum variant
    for fd in group_fns {
        let fn_name = aver_name_to_rust(&fd.name);
        let variant = fn_name_to_variant(&fd.name);
        let params = emit_fn_params(&fd.params, false);
        // Enum variant args: only non-rc-wrapped params.
        // Borrowed params (`&T` from borrow-by-default) need `.clone()` to produce owned T for enum.
        let variant_arg_names: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, type_ann)| {
                let rust_name = aver_name_to_rust(name);
                let ty = parse_type_str(type_ann);
                if should_borrow_param(&ty) {
                    // Borrowed param: clone to get owned value for enum variant
                    format!("{}.clone()", rust_name)
                } else {
                    rust_name
                }
            })
            .collect();
        let variant_call = if variant_arg_names.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!(
                "{}::{}({})",
                enum_name,
                variant,
                variant_arg_names.join(", ")
            )
        };

        // Build the borrowed extra args for the trampoline call (owned → &T)
        let rc_extra_args: String = if !rc_names.is_empty() {
            let parts: Vec<String> = fd
                .params
                .iter()
                .filter(|(name, _)| rc_names.contains(name))
                .map(|(name, _)| format!("&{}", aver_name_to_rust(name)))
                .collect();
            if parts.is_empty() {
                String::new()
            } else {
                format!(", {}", parts.join(", "))
            }
        } else {
            String::new()
        };

        let mut wrapper = Vec::new();
        if let Some(desc) = &fd.desc {
            wrapper.push(format!("/// {}", desc));
        }
        wrapper.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        wrapper.push(format!(
            "    {}({}{})",
            trampoline_name, variant_call, rc_extra_args
        ));
        wrapper.push("}".to_string());
        sections.push(wrapper.join("\n"));
    }

    sections.join("\n\n")
}

fn emit_trampoline_arm_body(
    fd: &FnDef,
    enum_name: &str,
    member_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
) -> String {
    let stmts = fd.body.stmts();
    let mut lines = Vec::new();
    lines.push("                crate::cancel_checkpoint();".to_string());
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        match stmt {
            Stmt::Binding(name, _, expr) => {
                lines.push(format!(
                    "                let {} = {};",
                    aver_name_to_rust(name),
                    emit_expr(&expr.node, ctx, ectx)
                ));
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!(
                        "                {}",
                        emit_trampoline_expr(
                            &expr.node,
                            enum_name,
                            member_names,
                            ctx,
                            ectx,
                            rc_indices,
                        )
                    ));
                } else {
                    lines.push(format!(
                        "                {};",
                        emit_expr(&expr.node, ctx, ectx)
                    ));
                }
            }
        }
    }
    lines.join("\n")
}

/// Emit an expression in the trampoline context.
///
/// Tail calls to group members produce enum variants (bounce).
/// Non-tail expressions use `return` to exit the trampoline.
#[allow(clippy::too_many_arguments)]
fn emit_trampoline_expr(
    expr: &Expr,
    enum_name: &str,
    member_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
) -> String {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if member_names.contains(target) {
                // Bounce → produce enum variant (excluding Rc-wrapped args)
                let variant = fn_name_to_variant(target);
                let bare_args: Vec<Expr> = args.iter().map(|a| a.node.clone()).collect();
                let arg_strs: Vec<String> = bare_args
                    .iter()
                    .filter(|a| {
                        // Skip args that are pass-through Rc params (Ident matching an rc_wrapped name)
                        !matches!(a, Expr::Ident(name) if ectx.is_rc_wrapped(name))
                    })
                    .map(|a| clone_arg(a, ctx, ectx))
                    .collect();
                if arg_strs.is_empty() {
                    format!("{}::{}", enum_name, variant)
                } else {
                    format!("{}::{}({})", enum_name, variant, arg_strs.join(", "))
                }
            } else {
                // External tail call → regular call + return
                format!("return {}", emit_expr(expr, ctx, ectx))
            }
        }
        Expr::Match { subject, arms, .. } => {
            let subj = clone_arg(&subject.node, ctx, ectx);
            let dispatch_plan = classify_dispatch_plan_for_rust(arms, ctx, ectx);

            // Bool match → if/else
            if let Some(code) = try_emit_trampoline_bool_if_else(
                &subj,
                arms,
                enum_name,
                member_names,
                ctx,
                ectx,
                rc_indices,
            ) {
                return code;
            }

            // List match
            if super::expr::has_list_patterns(arms) {
                return super::expr::emit_list_match(subj, arms, None, true, ctx, |arm| {
                    emit_trampoline_expr(
                        &arm.body.node,
                        enum_name,
                        member_names,
                        ctx,
                        ectx,
                        rc_indices,
                    )
                });
            }

            if let Some(crate::ir::MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
                return emit_dispatch_table_match(subj, arms, shape, |arm| {
                    emit_trampoline_expr(
                        &arm.body.node,
                        enum_name,
                        member_names,
                        ctx,
                        ectx,
                        rc_indices,
                    )
                });
            }

            let needs_as_str = super::expr::has_string_literal_patterns(arms);
            let match_expr = if needs_as_str {
                format!("&*{}", subj)
            } else {
                subj
            };

            let mut arm_strs = Vec::new();
            for arm in arms {
                let pat = super::pattern::emit_pattern(&arm.pattern, needs_as_str, ctx);
                let body = emit_trampoline_expr(
                    &arm.body.node,
                    enum_name,
                    member_names,
                    ctx,
                    ectx,
                    rc_indices,
                );

                let mut rebinding_lines: Vec<String> = Vec::new();
                if let Pattern::Cons(head, _) = &arm.pattern
                    && head != "_"
                {
                    let h = aver_name_to_rust(head);
                    rebinding_lines.push(format!("let {} = {}.clone();", h, h));
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
                arm_strs.push(format!("                {} => {}", pat, rebindings));
            }

            format!(
                "match {} {{\n{}\n                }}",
                match_expr,
                arm_strs.join(",\n")
            )
        }
        _ => {
            // Non-tail expression → return to exit trampoline.
            // If this is a bare pass-through ident (&T), deref+clone to get owned T.
            if let Expr::Ident(name) = expr
                && ectx.is_rc_wrapped(name)
            {
                let code = emit_expr(expr, ctx, ectx);
                return format!("return (*{}).clone()", code);
            }
            format!("return {}", emit_expr(expr, ctx, ectx))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn try_emit_trampoline_bool_if_else(
    subj: &str,
    arms: &[MatchArm],
    enum_name: &str,
    member_names: &HashSet<String>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
) -> Option<String> {
    if arms.len() != 2 {
        return None;
    }
    let (true_body, false_body) = match (&arms[0].pattern, &arms[1].pattern) {
        (Pattern::Literal(Literal::Bool(true)), Pattern::Literal(Literal::Bool(false))) => {
            (&arms[0].body, &arms[1].body)
        }
        (Pattern::Literal(Literal::Bool(false)), Pattern::Literal(Literal::Bool(true))) => {
            (&arms[1].body, &arms[0].body)
        }
        _ => return None,
    };
    let t = emit_trampoline_expr(
        &true_body.node,
        enum_name,
        member_names,
        ctx,
        ectx,
        rc_indices,
    );
    let f = emit_trampoline_expr(
        &false_body.node,
        enum_name,
        member_names,
        ctx,
        ectx,
        rc_indices,
    );
    Some(format!("if {} {{ {} }} else {{ {} }}", subj, t, f))
}

/// Emit a memoized function with thread_local cache.
fn emit_memo_fn(
    fd: &FnDef,
    fn_name: &str,
    _params_str: &str,
    ret_type: &str,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    visibility: &str,
) -> String {
    let cache_name = fn_name.to_uppercase() + "_CACHE";

    // Build the key type and value type
    let param_types: Vec<String> = fd
        .params
        .iter()
        .map(|(_, ty)| type_annotation_to_rust(ty))
        .collect();
    let param_key_types: Vec<crate::types::Type> = fd
        .params
        .iter()
        .map(|(_, ty)| crate::types::parse_type_str(ty))
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
        memo_key_component_expr(&param_names[0], &param_key_types[0])
    } else {
        let parts: Vec<String> = param_names
            .iter()
            .zip(param_key_types.iter())
            .map(|(name, ty)| memo_key_component_expr(name, ty))
            .collect();
        format!("({},)", parts.join(", "))
    };

    let params = emit_fn_params(&fd.params, false);

    let mut out = String::new();
    writeln!(out, "thread_local! {{").unwrap();
    writeln!(
        out,
        "    static {}: std::cell::RefCell<std::collections::HashMap<{}, {}>> = std::cell::RefCell::new(std::collections::HashMap::new());",
        cache_name, key_type, ret_type
    )
    .unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "{}fn {}({}) -> {} {{",
        visibility, fn_name, params, ret_type
    )
    .unwrap();
    writeln!(out, "    {}.with(|cache| {{", cache_name).unwrap();
    writeln!(out, "        let __memo_key = {};", key_expr).unwrap();
    writeln!(
        out,
        "        if let Some(r) = cache.borrow().get(&__memo_key).cloned() {{ return r; }}"
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
        "        cache.borrow_mut().insert(__memo_key, __result.clone());"
    )
    .unwrap();
    writeln!(out, "        __result").unwrap();
    writeln!(out, "    }})").unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
}

fn emit_memo_inner_body(body: &FnBody, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let stmts = body.stmts();
    let mut parts = Vec::new();
    parts.push("crate::cancel_checkpoint();".to_string());
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        match stmt {
            Stmt::Binding(_, _, _) => parts.push(emit_stmt(stmt, ctx, ectx)),
            Stmt::Expr(expr) => {
                if is_last {
                    parts.push(emit_expr(&expr.node, ctx, ectx));
                } else {
                    parts.push(format!("{};", emit_expr(&expr.node, ctx, ectx)));
                }
            }
        }
    }
    parts.join(" ")
}

/// Emit the main function, incorporating top-level statements.
#[allow(dead_code)]
pub fn emit_main(main_fn: Option<&FnDef>, top_stmts: &[&Stmt], ctx: &CodegenContext) -> String {
    emit_main_with_visibility(main_fn, top_stmts, ctx, false)
}

pub fn emit_public_main(
    main_fn: Option<&FnDef>,
    top_stmts: &[&Stmt],
    ctx: &CodegenContext,
) -> String {
    emit_main_with_visibility(main_fn, top_stmts, ctx, true)
}

fn emit_main_with_visibility(
    main_fn: Option<&FnDef>,
    top_stmts: &[&Stmt],
    ctx: &CodegenContext,
    public: bool,
) -> String {
    let mut out = String::new();
    let ectx = EmitCtx::empty();
    let visibility = visibility_prefix(public);

    // Check if main returns a Result (needed for ? operator support)
    let returns_result = main_fn.is_some_and(|fd| fd.return_type.starts_with("Result<"));

    if returns_result {
        let ret_type = type_annotation_to_rust(&main_fn.unwrap().return_type);
        writeln!(out, "{}fn main() -> {} {{", visibility, ret_type).unwrap();
    } else {
        writeln!(out, "{}fn main() {{", visibility).unwrap();
    }

    let guest_wrap_main = ctx.emit_replay_runtime && ctx.guest_entry.as_deref() == Some("main");
    if guest_wrap_main {
        if returns_result {
            writeln!(
                out,
                "    aver_replay::with_guest_scope_result(\"main\", serde_json::Value::Null, || {{"
            )
            .unwrap();
        } else {
            writeln!(
                out,
                "    aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, || {{"
            )
            .unwrap();
        }
    }

    // Top-level statements first
    for stmt in top_stmts {
        let indent = if guest_wrap_main { "        " } else { "    " };
        writeln!(out, "{}{}", indent, emit_stmt(stmt, ctx, &ectx)).unwrap();
    }

    // Main function body
    if let Some(fd) = main_fn {
        let main_ectx = build_fn_ectx(fd, ctx);
        let stmts = fd.body.stmts();
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last && returns_result {
                match stmt {
                    Stmt::Binding(_, _, _) => {
                        let indent = if guest_wrap_main { "        " } else { "    " };
                        writeln!(out, "{}{}", indent, emit_stmt(stmt, ctx, &main_ectx)).unwrap();
                    }
                    Stmt::Expr(expr) => {
                        let indent = if guest_wrap_main { "        " } else { "    " };
                        writeln!(out, "{}{}", indent, emit_expr(&expr.node, ctx, &main_ectx))
                            .unwrap();
                    }
                }
            } else {
                let indent = if guest_wrap_main { "        " } else { "    " };
                writeln!(out, "{}{}", indent, emit_stmt(stmt, ctx, &main_ectx)).unwrap();
            }
        }
    }

    if guest_wrap_main {
        writeln!(out, "    }})").unwrap();
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
            let left_str = emit_expr(&left.node, ctx, &ectx);
            let right_str = emit_expr(&right.node, ctx, &ectx);

            // Check if either side uses `?` operator
            let uses_error_prop =
                expr_uses_error_prop(&left.node) || expr_uses_error_prop(&right.node);

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
    use crate::ast::{
        BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, TypeDef, TypeVariant,
    };
    use crate::codegen::CodegenContext;
    use crate::types::Type;
    use std::collections::{HashMap, HashSet};
    use std::sync::Arc as Rc;

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
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            extra_fn_defs: Vec::new(),
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            fn_analyses: HashMap::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
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
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Literal(
                crate::ast::Literal::Int(0),
            )))),
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
        let expr = Expr::TailCall(Box::new(TailCallData::new(
            "repeatSum".to_string(),
            vec![
                Spanned::bare(Expr::Ident("xs".to_string())),
                Spanned::bare(Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Spanned::bare(Expr::Ident("remaining".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(crate::ast::Literal::Int(1)))),
                )),
                Spanned::bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Spanned::bare(Expr::Ident("sink".to_string()))),
                    Box::new(Spanned::bare(Expr::FnCall(
                        Box::new(Spanned::bare(Expr::Ident("sumList".to_string()))),
                        vec![
                            Spanned::bare(Expr::Ident("xs".to_string())),
                            Spanned::bare(Expr::Literal(crate::ast::Literal::Int(0))),
                        ],
                    ))),
                )),
            ],
        )));

        let passthrough = HashSet::from([0usize]);
        let code = emit_tco_expr(
            &expr,
            &fd.name,
            &fd.params,
            &ctx,
            &ectx,
            &HashSet::new(),
            &passthrough,
            &HashMap::new(),
        );
        assert!(!code.contains("let __tmp0 = xs.clone();"));
        assert!(code.contains("let __tmp1 = (remaining - 1i64);"));
        // Numeric add no longer uses &rhs; list param gets .clone() when reused
        assert!(code.contains("let __tmp2 = (sink + sumList(xs.clone(), 0i64));"));
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
        let expr = Expr::TailCall(Box::new(TailCallData::new(
            "repeatAppend".to_string(),
            vec![
                Spanned::bare(Expr::Ident("a".to_string())),
                Spanned::bare(Expr::Ident("b".to_string())),
                Spanned::bare(Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Spanned::bare(Expr::Ident("remaining".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(crate::ast::Literal::Int(1)))),
                )),
                Spanned::bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Spanned::bare(Expr::Ident("sink".to_string()))),
                    Box::new(Spanned::bare(Expr::FnCall(
                        Box::new(Spanned::bare(Expr::Ident("List.len".to_string()))),
                        vec![Spanned::bare(Expr::FnCall(
                            Box::new(Spanned::bare(Expr::Ident("appendLists".to_string()))),
                            vec![
                                Spanned::bare(Expr::Ident("a".to_string())),
                                Spanned::bare(Expr::Ident("b".to_string())),
                            ],
                        ))],
                    ))),
                )),
            ],
        )));

        let passthrough = HashSet::from([0usize, 1usize]);
        let code = emit_tco_expr(
            &expr,
            &fd.name,
            &fd.params,
            &ctx,
            &ectx,
            &HashSet::new(),
            &passthrough,
            &HashMap::new(),
        );
        assert!(!code.contains("let __tmp0 = a.clone();"));
        assert!(!code.contains("let __tmp1 = b.clone();"));
        // Numeric add no longer uses &rhs; list params get .clone() when reused
        assert!(
            code.contains(
                "let __tmp3 = (sink + (appendLists(a.clone(), b.clone()).len() as i64));"
            )
        );
    }

    #[test]
    fn self_tco_does_not_rewrite_same_arity_mutual_tailcall() {
        let ctx = empty_ctx();
        let fd = list_param_fn("validSymbolNames", vec![("e", "Sexpr")]);
        let ectx = build_fn_ectx(&fd, &ctx);
        let expr = Expr::TailCall(Box::new(TailCallData::new(
            "validSymbolList".to_string(),
            vec![Spanned::bare(Expr::Ident("e".to_string()))],
        )));

        let passthrough = HashSet::new();
        let code = emit_tco_expr(
            &expr,
            &fd.name,
            &fd.params,
            &ctx,
            &ectx,
            &HashSet::new(),
            &passthrough,
            &HashMap::new(),
        );
        // Borrowed param gets .clone() when passed to another function
        assert_eq!(code, "validSymbolList(e.clone())");
        assert!(!code.contains("continue"));
    }

    #[test]
    fn self_tco_skips_rebinding_copy_passthrough_params() {
        let ctx = empty_ctx();
        let fd = list_param_fn(
            "sumAreas",
            vec![("n", "Int"), ("acc", "Int"), ("pick", "Int")],
        );
        let ectx = build_fn_ectx(&fd, &ctx);
        let expr = Expr::TailCall(Box::new(TailCallData::new(
            "sumAreas".to_string(),
            vec![
                Spanned::bare(Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(crate::ast::Literal::Int(1)))),
                )),
                Spanned::bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Spanned::bare(Expr::Ident("acc".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(crate::ast::Literal::Int(1)))),
                )),
                Spanned::bare(Expr::Ident("pick".to_string())),
            ],
        )));

        let passthrough = HashSet::from([2usize]);
        let code = emit_tco_expr(
            &expr,
            &fd.name,
            &fd.params,
            &ctx,
            &ectx,
            &HashSet::new(),
            &passthrough,
            &HashMap::new(),
        );
        assert!(!code.contains("let __tmp2 = pick;"));
        assert!(!code.contains("pick = __tmp2;"));
        assert!(code.contains("continue;"));
    }

    #[test]
    fn optimized_tco_hoists_invariant_pure_call_chain_over_passthrough_param() {
        let helper_tag = FnDef {
            name: "tag".to_string(),
            line: 1,
            params: vec![("pick".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("pick".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(1)),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(10)))), binding_slots: std::sync::OnceLock::new() },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(20)))), binding_slots: std::sync::OnceLock::new() },
                ],
            }))),
            resolution: None,
        };
        let helper_score = FnDef {
            name: "score".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::BinOp(
                BinOp::Add,
                Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
            )))),
            resolution: None,
        };
        let fd = FnDef {
            name: "sumAreas".to_string(),
            line: 1,
            params: vec![
                ("n".to_string(), "Int".to_string()),
                ("acc".to_string(), "Int".to_string()),
                ("pick".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Spanned::bare(Expr::Ident("acc".to_string()))), binding_slots: std::sync::OnceLock::new() },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::bare(Expr::TailCall(Box::new(TailCallData::new(
                            "sumAreas".to_string(),
                            vec![
                                Spanned::bare(Expr::BinOp(
                                    BinOp::Sub,
                                    Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                                    Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                                )),
                                Spanned::bare(Expr::BinOp(
                                    BinOp::Add,
                                    Box::new(Spanned::bare(Expr::Ident("acc".to_string()))),
                                    Box::new(Spanned::bare(Expr::FnCall(
                                        Box::new(Spanned::bare(Expr::Ident("score".to_string()))),
                                        vec![Spanned::bare(Expr::FnCall(
                                            Box::new(Spanned::bare(Expr::Ident("tag".to_string()))),
                                            vec![Spanned::bare(Expr::Ident("pick".to_string()))],
                                        ))],
                                    ))),
                                )),
                                Spanned::bare(Expr::Ident("pick".to_string())),
                            ],
                        ))))), binding_slots: std::sync::OnceLock::new() },
                ],
            }))),
            resolution: None,
        };

        let mut ctx = empty_ctx();
        ctx.fn_defs = vec![helper_tag.clone(), helper_score.clone(), fd.clone()];
        ctx.fn_sigs
            .insert("tag".to_string(), (vec![Type::Int], Type::Int, vec![]));
        ctx.fn_sigs
            .insert("score".to_string(), (vec![Type::Int], Type::Int, vec![]));
        ctx.fn_sigs.insert(
            "sumAreas".to_string(),
            (vec![Type::Int, Type::Int, Type::Int], Type::Int, vec![]),
        );

        let emitted = emit_public_fn_def(&fd, false, &ctx);
        assert!(emitted.contains("let __aver_inv0 = score(tag(pick));"));
        // Numeric add no longer uses &rhs
        assert!(emitted.contains("let __tmp1 = (acc + __aver_inv0);"));
        assert!(!emitted.contains("pick = __tmp2;"));
    }

    #[test]
    fn recursive_sum_type_used_by_memo_can_derive_eq_hash() {
        let td = TypeDef::Sum {
            name: "Tree".to_string(),
            variants: vec![
                TypeVariant {
                    name: "Empty".to_string(),
                    fields: vec![],
                },
                TypeVariant {
                    name: "Node".to_string(),
                    fields: vec!["Tree".to_string(), "Int".to_string(), "Tree".to_string()],
                },
            ],
            line: 1,
        };
        let mut ctx = empty_ctx();
        ctx.type_defs.push(td.clone());

        let emitted = emit_public_type_def(&td, &ctx);
        assert!(emitted.contains("#[derive(Clone, Debug, PartialEq, Eq, Hash)]"));
    }

    #[test]
    fn float_param_fn_does_not_use_rust_memo_cache() {
        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Float".to_string())],
            return_type: "Float".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Ident(
                "x".to_string(),
            )))),
            resolution: None,
        };
        let mut ctx = empty_ctx();
        ctx.fn_sigs
            .insert("f".to_string(), (vec![Type::Float], Type::Float, vec![]));

        let emitted = emit_public_fn_def(&fd, true, &ctx);
        assert!(!emitted.contains("thread_local!"));
    }

    #[test]
    fn memoized_named_param_clones_cache_key_before_body() {
        let td = TypeDef::Sum {
            name: "Tree".to_string(),
            variants: vec![
                TypeVariant {
                    name: "Empty".to_string(),
                    fields: vec![],
                },
                TypeVariant {
                    name: "Node".to_string(),
                    fields: vec!["Tree".to_string(), "Int".to_string(), "Tree".to_string()],
                },
            ],
            line: 1,
        };
        let fd = FnDef {
            name: "member".to_string(),
            line: 1,
            params: vec![("t".to_string(), "Tree".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("t".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor("Tree.Empty".to_string(), vec![]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Bool(false)))), binding_slots: std::sync::OnceLock::new() },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Bool(true)))), binding_slots: std::sync::OnceLock::new() },
                ],
            }))),
            resolution: None,
        };

        let mut ctx = empty_ctx();
        ctx.type_defs.push(td);
        ctx.fn_sigs.insert(
            "member".to_string(),
            (vec![Type::Named("Tree".to_string())], Type::Bool, vec![]),
        );

        let emitted = emit_public_fn_def(&fd, true, &ctx);
        assert!(emitted.contains("let __memo_key = t.clone();"));
        assert!(emitted.contains("get(&__memo_key)"));
        assert!(emitted.contains("insert(__memo_key, __result.clone())"));
    }

    #[test]
    fn mutual_tco_generates_trampoline_for_two_functions() {
        let is_even = FnDef {
            name: "isEven".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Match {
                subject: Box::new(Spanned::bare(Expr::BinOp(
                    BinOp::Eq,
                    Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                ))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Bool(true)))), binding_slots: std::sync::OnceLock::new() },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        body: Box::new(Spanned::bare(Expr::TailCall(Box::new(TailCallData::new(
                            "isOdd".to_string(),
                            vec![Spanned::bare(Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                                Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                            ))],
                        ))))), binding_slots: std::sync::OnceLock::new() },
                ],
            }))),
            resolution: None,
        };

        let is_odd = FnDef {
            name: "isOdd".to_string(),
            line: 5,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Match {
                subject: Box::new(Spanned::bare(Expr::BinOp(
                    BinOp::Eq,
                    Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                    Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                ))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Bool(false)))), binding_slots: std::sync::OnceLock::new() },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        body: Box::new(Spanned::bare(Expr::TailCall(Box::new(TailCallData::new(
                            "isEven".to_string(),
                            vec![Spanned::bare(Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                                Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                            ))],
                        ))))), binding_slots: std::sync::OnceLock::new() },
                ],
            }))),
            resolution: None,
        };

        let fn_defs: Vec<&FnDef> = vec![&is_even, &is_odd];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert_eq!(groups.len(), 1, "should find one mutual TCO group");
        assert_eq!(groups[0], vec![0, 1]);

        let ctx = empty_ctx();
        let block = emit_mutual_tco_block(1, &fn_defs, &ctx, "pub ");

        // Enum with variants for both functions
        assert!(block.contains("enum __MutualTco1"));
        assert!(block.contains("IsEven(i64)"));
        assert!(block.contains("IsOdd(i64)"));

        // Trampoline dispatch loop
        assert!(block.contains("fn __mutual_tco_trampoline_1"));
        assert!(block.contains("loop {"));
        assert!(block.contains("__state = match __state"));

        // Bounce: TailCall becomes enum variant (not a regular call)
        assert!(block.contains("__MutualTco1::IsOdd("));
        assert!(block.contains("__MutualTco1::IsEven("));

        // Non-tail returns exit the trampoline
        assert!(block.contains("return true"));
        assert!(block.contains("return false"));

        // Thin wrappers
        assert!(block.contains("pub fn isEven(n: i64) -> bool"));
        assert!(block.contains("pub fn isOdd(n: i64) -> bool"));
        assert!(block.contains("__mutual_tco_trampoline_1(__MutualTco1::IsEven(n))"));
        assert!(block.contains("__mutual_tco_trampoline_1(__MutualTco1::IsOdd(n))"));
    }

    #[test]
    fn mutual_tco_three_functions_single_group() {
        let make_fn = |name: &str, target: &str| FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "String".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::TailCall(Box::new(
                TailCallData::new(
                    target.to_string(),
                    vec![Spanned::bare(Expr::Ident("n".to_string()))],
                ),
            ))))),
            resolution: None,
        };

        let a = make_fn("stateA", "stateB");
        let b = make_fn("stateB", "stateC");
        let c = make_fn("stateC", "stateA");

        let fn_defs: Vec<&FnDef> = vec![&a, &b, &c];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0], vec![0, 1, 2]);

        let ctx = empty_ctx();
        let block = emit_mutual_tco_block(1, &fn_defs, &ctx, "pub ");
        assert!(block.contains("StateA(i64)"));
        assert!(block.contains("StateB(i64)"));
        assert!(block.contains("StateC(i64)"));
    }

    #[test]
    fn one_way_tailcall_chain_is_not_a_mutual_group() {
        let make_tail_fn = |name: &str, target: &str| FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "String".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::TailCall(Box::new(
                TailCallData::new(
                    target.to_string(),
                    vec![Spanned::bare(Expr::Ident("n".to_string()))],
                ),
            ))))),
            resolution: None,
        };

        let a = make_tail_fn("stateA", "stateB");
        let b = make_tail_fn("stateB", "stateC");
        let c = FnDef {
            name: "stateC".to_string(),
            line: 3,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "String".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Literal(
                Literal::Str("done".to_string()),
            )))),
            resolution: None,
        };

        let fn_defs: Vec<&FnDef> = vec![&a, &b, &c];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert!(
            groups.is_empty(),
            "one-way tailcall chain should not create a mutual trampoline group"
        );
    }

    #[test]
    fn self_only_tco_not_included_in_mutual_groups() {
        let self_rec = FnDef {
            name: "factorial".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::TailCall(Box::new(
                TailCallData::new(
                    "factorial".to_string(),
                    vec![Spanned::bare(Expr::Ident("n".to_string()))],
                ),
            ))))),
            resolution: None,
        };

        let fn_defs: Vec<&FnDef> = vec![&self_rec];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert!(
            groups.is_empty(),
            "self-only TCO should not create a mutual group"
        );
    }

    #[test]
    fn optimized_forward_wrapper_gets_inline_always() {
        let fd = FnDef {
            name: "swap".to_string(),
            line: 1,
            params: vec![
                ("a".to_string(), "Int".to_string()),
                ("b".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::FnCall(
                Box::new(Spanned::bare(Expr::Ident("first".to_string()))),
                vec![
                    Spanned::bare(Expr::Ident("b".to_string())),
                    Spanned::bare(Expr::Ident("a".to_string())),
                ],
            )))),
            resolution: None,
        };

        let mut semantic_ctx = empty_ctx();
        semantic_ctx.fn_sigs.insert(
            "swap".to_string(),
            (vec![Type::Int, Type::Int], Type::Int, vec![]),
        );
        semantic_ctx.fn_sigs.insert(
            "first".to_string(),
            (vec![Type::Int, Type::Int], Type::Int, vec![]),
        );
        let semantic = emit_public_fn_def(&fd, false, &semantic_ctx);
        // Both modes now emit #[inline(always)] for thin wrappers
        assert!(semantic.contains("#[inline(always)]"));
        assert!(semantic.contains("pub fn swap(a: i64, b: i64) -> i64"));
        assert!(semantic.contains("first(b, a)"));
    }

    #[test]
    fn optimized_leaf_wrapper_uses_body_plan_and_gets_inline_always() {
        let fd = FnDef {
            name: "cellAt".to_string(),
            line: 1,
            params: vec![
                ("grid".to_string(), "Vector<Int>".to_string()),
                ("idx".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::FnCall(
                Box::new(Spanned::bare(Expr::Attr(
                    Box::new(Spanned::bare(Expr::Ident("Option".to_string()))),
                    "withDefault".to_string(),
                ))),
                vec![
                    Spanned::bare(Expr::FnCall(
                        Box::new(Spanned::bare(Expr::Attr(
                            Box::new(Spanned::bare(Expr::Ident("Vector".to_string()))),
                            "get".to_string(),
                        ))),
                        vec![
                            Spanned::bare(Expr::Ident("grid".to_string())),
                            Spanned::bare(Expr::Ident("idx".to_string())),
                        ],
                    )),
                    Spanned::bare(Expr::Literal(Literal::Int(0))),
                ],
            )))),
            resolution: None,
        };

        let mut semantic_ctx = empty_ctx();
        semantic_ctx.fn_sigs.insert(
            "cellAt".to_string(),
            (
                vec![Type::Vector(Box::new(Type::Int)), Type::Int],
                Type::Int,
                vec![],
            ),
        );
        let semantic = emit_public_fn_def(&fd, false, &semantic_ctx);
        // Both modes now emit #[inline(always)] for leaf wrappers
        assert!(semantic.contains("#[inline(always)]"));
        // Vector param is now borrowed
        assert!(
            semantic.contains("pub fn cellAt(grid: &aver_rt::AverVector<i64>, idx: i64) -> i64")
        );
        assert!(semantic.contains("grid.get(idx as usize).cloned().unwrap_or(0i64)"));
    }

    #[test]
    fn optimized_binding_block_uses_body_plan_and_gets_inline_always() {
        let fd = FnDef {
            name: "cellAtPlusOne".to_string(),
            line: 1,
            params: vec![
                ("grid".to_string(), "Vector<Int>".to_string()),
                ("idx".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Block(vec![
                Stmt::Binding(
                    "cell".to_string(),
                    None,
                    Spanned::bare(Expr::FnCall(
                        Box::new(Spanned::bare(Expr::Attr(
                            Box::new(Spanned::bare(Expr::Ident("Option".to_string()))),
                            "withDefault".to_string(),
                        ))),
                        vec![
                            Spanned::bare(Expr::FnCall(
                                Box::new(Spanned::bare(Expr::Attr(
                                    Box::new(Spanned::bare(Expr::Ident("Vector".to_string()))),
                                    "get".to_string(),
                                ))),
                                vec![
                                    Spanned::bare(Expr::Ident("grid".to_string())),
                                    Spanned::bare(Expr::Ident("idx".to_string())),
                                ],
                            )),
                            Spanned::bare(Expr::Literal(Literal::Int(0))),
                        ],
                    )),
                ),
                Stmt::Expr(Spanned::bare(Expr::FnCall(
                    Box::new(Spanned::bare(Expr::Attr(
                        Box::new(Spanned::bare(Expr::Ident("Int".to_string()))),
                        "max".to_string(),
                    ))),
                    vec![
                        Spanned::bare(Expr::Ident("cell".to_string())),
                        Spanned::bare(Expr::Literal(Literal::Int(1))),
                    ],
                ))),
            ])),
            resolution: None,
        };

        let mut semantic_ctx = empty_ctx();
        semantic_ctx.fn_sigs.insert(
            "cellAtPlusOne".to_string(),
            (
                vec![Type::Vector(Box::new(Type::Int)), Type::Int],
                Type::Int,
                vec![],
            ),
        );
        let semantic = emit_public_fn_def(&fd, false, &semantic_ctx);
        // Both modes now emit #[inline(always)] for binding-block wrappers
        assert!(semantic.contains("#[inline(always)]"));
        assert!(semantic.contains("let cell = grid.get(idx as usize).cloned().unwrap_or(0i64);"));
        assert!(semantic.contains("cell.max(1i64)"));
    }
}

use super::expr::{aver_name_to_rust, clone_arg, emit_expr, emit_stmt};
use super::liveness::{
    EmitCtx, collect_vars, compute_args_used_after_with_rc, compute_block_used_after,
    compute_block_used_after_with_rc, is_copy_type,
};
use super::types::type_annotation_to_rust;
use crate::ast::*;
use crate::codegen::CodegenContext;
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

fn type_def_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

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
        Type::Map(_, _) | Type::Fn(_, _, _) | Type::Unknown => false,
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
                        format!("std::rc::Rc<{}>", rust_ty)
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

    let ectx = build_fn_ectx(fd, ctx);
    let use_memo = is_memo && fn_supports_rust_memo(fd, ctx);
    let is_guest_entry = ctx.guest_entry.as_deref() == Some(fd.name.as_str());
    let guest_args_name = if is_guest_entry {
        guest_args_param(fd)
    } else {
        None
    };
    let self_host_state = if is_guest_entry && ctx.emit_self_host_runtime {
        self_host_runtime_state(fd)
    } else {
        None
    };

    if ctx.emit_replay_runtime && is_guest_entry {
        lines.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        match &guest_args_name {
            Some(guest_args) => {
                lines.push(format!(
                    "    let __replay_input = aver_replay::ReplayValue::to_replay_json(&{});",
                    guest_args
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
                        format!(
                            "aver_replay::ReplayValue::to_replay_json(&{})",
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
        if let Some((prog_name, module_fns_name)) = &self_host_state {
            lines.push(format!(
                "        let __self_host_fns = crate::aver_generated::domain::eval::fnsToStore(aver_rt::AverList::concat(&{}.clone(), &{}.fns.clone()));",
                module_fns_name, prog_name
            ));
            lines.push("        crate::with_self_host_fn_store(__self_host_fns, || {".to_string());
            lines.push(indent_block(&emit_fn_body(&fd.body, ctx, &ectx), 3));
            lines.push("        })".to_string());
        } else {
            lines.push(emit_fn_body(&fd.body, ctx, &ectx));
        }
        lines.push("    })".to_string());
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
                // Rc-wrapped param: not mutable (it's shared via Rc)
                format!("{}: std::rc::Rc<{}>", rust_name, rust_type)
            } else if mutable {
                format!("mut {}: {}", rust_name, rust_type)
            } else {
                format!("{}: {}", rust_name, rust_type)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_fn_body(body: &FnBody, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let stmts = body.stmts();
    // Compute per-statement used_after sets
    let stmt_ctxs = compute_block_used_after(stmts, &ectx.used_after, &ectx.local_types);
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        let sctx = &stmt_ctxs[i];
        match stmt {
            Stmt::Binding(name, type_ann, _) => {
                lines.push(format!("    {}", emit_stmt(stmt, ctx, sctx)));
                let _ = (name, type_ann);
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!("    {}", emit_expr(expr, ctx, sctx)));
                } else {
                    lines.push(format!("    {};", emit_expr(expr, ctx, sctx)));
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
    body.stmts().iter().any(|s| match s {
        Stmt::Expr(e) => expr_has_self_tailcall(e, fn_name),
        Stmt::Binding(_, _, e) => expr_has_self_tailcall(e, fn_name),
    })
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
/// These can safely be wrapped in Rc<T> to avoid deep cloning.
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
                if !check_expr_passthrough_by_name(e, member_names, param_name, fn_param_map) {
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
            let (target, args) = boxed.as_ref();
            if !member_names.contains(target.as_str()) {
                return true; // call to non-member, irrelevant
            }
            // Find the index of param_name in the TARGET function
            if let Some(target_params) = fn_param_map.get(target.as_str())
                && let Some(&(target_idx, _)) = target_params.get(param_name)
            {
                // arg at target_idx must be Ident(param_name) from the caller
                target_idx < args.len()
                    && matches!(&args[target_idx], Expr::Ident(name) if name == param_name)
            } else {
                false
            }
        }
        Expr::Match { arms, .. } => arms.iter().all(|arm| {
            check_expr_passthrough_by_name(&arm.body, member_names, param_name, fn_param_map)
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
                check_expr_tailcalls_for_rc(e, member_names, params, candidates);
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
            let (target, args) = boxed.as_ref();
            if member_names.contains(target.as_str()) && args.len() == params.len() {
                // For each candidate index, check if arg[i] == Ident(param_name[i])
                let to_remove: Vec<usize> = candidates
                    .iter()
                    .copied()
                    .filter(|&i| !matches!(&args[i], Expr::Ident(name) if *name == params[i].0))
                    .collect();
                for idx in to_remove {
                    candidates.remove(&idx);
                }
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                check_expr_tailcalls_for_rc(&arm.body, member_names, params, candidates);
            }
        }
        _ => {}
    }
}

/// Build a set of param names that should be Rc-wrapped, given rc_indices.
fn rc_param_names(params: &[(String, String)], rc_indices: &HashSet<usize>) -> HashSet<String> {
    rc_indices
        .iter()
        .filter_map(|&i| params.get(i).map(|(name, _)| name.clone()))
        .collect()
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
    // Compute pass-through Rc params for self-TCO
    let rc_indices = compute_rc_params(&[fd], ctx);
    let rc_names = rc_param_names(&fd.params, &rc_indices);
    let ectx = if rc_names.is_empty() {
        ectx.clone()
    } else {
        ectx.with_rc_wrapped(rc_names)
    };

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
        let (name, ty) = &fd.params[i];
        let rust_name = aver_name_to_rust(name);
        let rust_type = type_annotation_to_rust(ty);
        lines.push(format!(
            "    let {} = std::rc::Rc::new({});",
            rust_name, rust_name
        ));
        let _ = rust_type;
    }

    lines.push("    loop {".to_string());

    // Emit body with TailCall → { reassign; continue }
    let body_code = emit_tco_body(&fd.body, &fd.name, &fd.params, ctx, &ectx, &rc_indices);
    lines.push(body_code);

    lines.push("    }".to_string());
    lines.push("}".to_string());
    lines.join("\n")
}

fn emit_tco_body(
    body: &FnBody,
    self_name: &str,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
) -> String {
    let stmts = body.stmts();
    let stmt_ctxs = compute_block_used_after_with_rc(
        stmts,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
    );
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
                        emit_tco_expr(expr, self_name, params, ctx, sctx, rc_indices)
                    ));
                } else {
                    lines.push(format!("        {};", emit_expr(expr, ctx, sctx)));
                }
            }
        }
    }
    lines.join("\n")
}

fn try_emit_tco_bool_if_else(
    subj: &str,
    arms: &[MatchArm],
    self_name: &str,
    params: &[(String, String)],
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
    let t = emit_tco_expr(true_body, self_name, params, ctx, ectx, rc_indices);
    let f = emit_tco_expr(false_body, self_name, params, ctx, ectx, rc_indices);
    Some(format!("if {} {{ {} }} else {{ {} }}", subj, t, f))
}

fn emit_tco_expr(
    expr: &Expr,
    self_name: &str,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    rc_indices: &HashSet<usize>,
) -> String {
    match expr {
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            if target != self_name || args.len() != params.len() {
                return emit_expr(expr, ctx, ectx);
            }

            // Self TCO — create temp vars, then reassign.
            // Skip Rc-wrapped params (they're pass-through, never rebound).
            let arg_ctxs = compute_args_used_after_with_rc(
                args,
                &std::collections::HashSet::new(),
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let arg_strs: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| clone_arg(a, ctx, ac))
                .collect();

            let mut lines = Vec::new();
            lines.push("{".to_string());
            for (i, arg_str) in arg_strs.iter().enumerate() {
                if rc_indices.contains(&i) {
                    continue; // pass-through Rc param — no rebinding needed
                }
                lines.push(format!("            let __tmp{} = {};", i, arg_str));
            }
            for (i, (name, _)) in params.iter().enumerate() {
                if rc_indices.contains(&i) {
                    continue; // pass-through Rc param — no rebinding needed
                }
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
            let subj = clone_arg(subject, ctx, &subj_ectx);

            // Bool match → if/else in TCO context
            if let Some(code) =
                try_emit_tco_bool_if_else(&subj, arms, self_name, params, ctx, ectx, rc_indices)
            {
                return code;
            }

            let needs_as_str = super::expr::has_string_literal_patterns(arms);
            if super::expr::has_list_patterns(arms) {
                return super::expr::emit_list_match(subj, arms, ctx, |arm| {
                    emit_tco_expr(&arm.body, self_name, params, ctx, ectx, rc_indices)
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
                let body = emit_tco_expr(&arm.body, self_name, params, ctx, ectx, rc_indices);
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

    // 1. Enum definition — exclude Rc-wrapped params (they're shared across iterations)
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

    // 2. Trampoline function — Rc-wrapped params are extra parameters
    let mut tramp_lines = Vec::new();

    // Build the Rc extra params for the trampoline signature (owned Rc<T>)
    // Use first fn that has them (all fns have same name+type for Rc params)
    let rc_extra_params: String = if !rc_names.is_empty() && !group_fns.is_empty() {
        let parts: Vec<String> = group_fns[0]
            .params
            .iter()
            .filter(|(name, _)| rc_names.contains(name))
            .map(|(name, ty)| {
                format!(
                    "{}: std::rc::Rc<{}>",
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

        let ectx = build_fn_ectx(fd, ctx);
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

    // 3. Wrapper functions — accept plain T, wrap Rc params in Rc::new(), call trampoline
    for fd in group_fns {
        let fn_name = aver_name_to_rust(&fd.name);
        let variant = fn_name_to_variant(&fd.name);
        let params = emit_fn_params(&fd.params, false);
        // Enum variant args: only non-Rc params
        let variant_arg_names: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, _)| aver_name_to_rust(name))
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

        // Build the Rc extra args for the trampoline call (owned Rc<T>)
        let rc_extra_args: String = if !rc_names.is_empty() {
            let parts: Vec<String> = fd
                .params
                .iter()
                .filter(|(name, _)| rc_names.contains(name))
                .map(|(name, _)| format!("std::rc::Rc::new({})", aver_name_to_rust(name)))
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
    let stmt_ctxs = compute_block_used_after_with_rc(
        stmts,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
    );
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        let sctx = &stmt_ctxs[i];
        match stmt {
            Stmt::Binding(name, _, expr) => {
                lines.push(format!(
                    "                let {} = {};",
                    aver_name_to_rust(name),
                    emit_expr(expr, ctx, sctx)
                ));
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!(
                        "                {}",
                        emit_trampoline_expr(expr, enum_name, member_names, ctx, sctx, rc_indices,)
                    ));
                } else {
                    lines.push(format!("                {};", emit_expr(expr, ctx, sctx)));
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
            let (target, args) = boxed.as_ref();
            if member_names.contains(target) {
                // Bounce → produce enum variant (excluding Rc-wrapped args)
                let variant = fn_name_to_variant(target);
                let arg_ctxs = compute_args_used_after_with_rc(
                    args,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                );
                let arg_strs: Vec<String> = args
                    .iter()
                    .zip(arg_ctxs.iter())
                    .filter(|(a, _)| {
                        // Skip args that are pass-through Rc params (Ident matching an rc_wrapped name)
                        !matches!(a, Expr::Ident(name) if ectx.is_rc_wrapped(name))
                    })
                    .map(|(a, ac)| clone_arg(a, ctx, ac))
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
            // Compute used_after for subject
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
                return super::expr::emit_list_match(subj, arms, ctx, |arm| {
                    emit_trampoline_expr(&arm.body, enum_name, member_names, ctx, ectx, rc_indices)
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
                let body =
                    emit_trampoline_expr(&arm.body, enum_name, member_names, ctx, ectx, rc_indices);

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
            // If this is a bare Rc-wrapped ident, deref+clone to get T.
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
    let t = emit_trampoline_expr(true_body, enum_name, member_names, ctx, ectx, rc_indices);
    let f = emit_trampoline_expr(false_body, enum_name, member_names, ctx, ectx, rc_indices);
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
        let stmt_ctxs =
            compute_block_used_after(stmts, &main_ectx.used_after, &main_ectx.local_types);
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            let sctx = &stmt_ctxs[i];
            if is_last && returns_result {
                match stmt {
                    Stmt::Binding(_, _, _) => {
                        let indent = if guest_wrap_main { "        " } else { "    " };
                        writeln!(out, "{}{}", indent, emit_stmt(stmt, ctx, sctx)).unwrap();
                    }
                    Stmt::Expr(expr) => {
                        let indent = if guest_wrap_main { "        " } else { "    " };
                        writeln!(out, "{}{}", indent, emit_expr(expr, ctx, sctx)).unwrap();
                    }
                }
            } else {
                let indent = if guest_wrap_main { "        " } else { "    " };
                writeln!(out, "{}{}", indent, emit_stmt(stmt, ctx, sctx)).unwrap();
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
    use crate::ast::{
        BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, TypeDef, TypeVariant,
    };
    use crate::codegen::CodegenContext;
    use crate::types::Type;
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
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_runtime: false,
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
            body: Rc::new(FnBody::from_expr(Expr::Literal(crate::ast::Literal::Int(
                0,
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

        let code = emit_tco_expr(&expr, &fd.name, &fd.params, &ctx, &ectx, &HashSet::new());
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

        let code = emit_tco_expr(&expr, &fd.name, &fd.params, &ctx, &ectx, &HashSet::new());
        assert!(code.contains("let __tmp0 = a.clone();"));
        assert!(code.contains("let __tmp1 = b.clone();"));
        assert!(code.contains("let __tmp3 = (sink + &(appendLists(a, b).len() as i64));"));
    }

    #[test]
    fn self_tco_does_not_rewrite_same_arity_mutual_tailcall() {
        let ctx = empty_ctx();
        let fd = list_param_fn("validSymbolNames", vec![("e", "Sexpr")]);
        let ectx = build_fn_ectx(&fd, &ctx);
        let expr = Expr::TailCall(Box::new((
            "validSymbolList".to_string(),
            vec![Expr::Ident("e".to_string())],
        )));

        let code = emit_tco_expr(&expr, &fd.name, &fd.params, &ctx, &ectx, &HashSet::new());
        assert_eq!(code, "validSymbolList(e)");
        assert!(!code.contains("continue"));
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
            body: Rc::new(FnBody::from_expr(Expr::Ident("x".to_string()))),
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
            body: Rc::new(FnBody::from_expr(Expr::Match {
                subject: Box::new(Expr::Ident("t".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor("Tree.Empty".to_string(), vec![]),
                        body: Box::new(Expr::Literal(Literal::Bool(false))),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::Literal(Literal::Bool(true))),
                    },
                ],
                line: 1,
            })),
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
            body: Rc::new(FnBody::from_expr(Expr::Match {
                subject: Box::new(Expr::BinOp(
                    BinOp::Eq,
                    Box::new(Expr::Ident("n".to_string())),
                    Box::new(Expr::Literal(Literal::Int(0))),
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Expr::Literal(Literal::Bool(true))),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        body: Box::new(Expr::TailCall(Box::new((
                            "isOdd".to_string(),
                            vec![Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Expr::Ident("n".to_string())),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )],
                        )))),
                    },
                ],
                line: 0,
            })),
            resolution: None,
        };

        let is_odd = FnDef {
            name: "isOdd".to_string(),
            line: 5,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Expr::Match {
                subject: Box::new(Expr::BinOp(
                    BinOp::Eq,
                    Box::new(Expr::Ident("n".to_string())),
                    Box::new(Expr::Literal(Literal::Int(0))),
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Expr::Literal(Literal::Bool(false))),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        body: Box::new(Expr::TailCall(Box::new((
                            "isEven".to_string(),
                            vec![Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Expr::Ident("n".to_string())),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )],
                        )))),
                    },
                ],
                line: 0,
            })),
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
            body: Rc::new(FnBody::from_expr(Expr::TailCall(Box::new((
                target.to_string(),
                vec![Expr::Ident("n".to_string())],
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
            body: Rc::new(FnBody::from_expr(Expr::TailCall(Box::new((
                target.to_string(),
                vec![Expr::Ident("n".to_string())],
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
            body: Rc::new(FnBody::from_expr(Expr::Literal(Literal::Str(
                "done".to_string(),
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
            body: Rc::new(FnBody::from_expr(Expr::TailCall(Box::new((
                "factorial".to_string(),
                vec![Expr::Ident("n".to_string())],
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
}

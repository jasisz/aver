/// Top-level Aver items → Lean 4 items (defs, inductives, structures, examples).
use std::collections::{HashMap, HashSet};

use super::expr::{aver_name_to_lean, emit_expr, emit_stmt};
use super::law_auto::emit_verify_law_forall_auto_proof;
use super::shared::to_lower_first;
use super::types::type_annotation_to_lean;
use super::{RecursionPlan, VerifyEmitMode};
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::expr_to_dotted_name;
use crate::verify_law::canonical_spec_ref;

/// Emit a Lean 4 type definition from an Aver TypeDef.
pub fn emit_type_def(td: &TypeDef) -> String {
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields),
    }
}

/// Check if a sum type is self-referencing (any variant field mentions the type name).
fn is_recursive_type(name: &str, variants: &[TypeVariant]) -> bool {
    for v in variants {
        for field in &v.fields {
            if field_type_contains(field, name) {
                return true;
            }
        }
    }
    false
}

/// Check if a product type is self-referencing.
fn is_recursive_product(name: &str, fields: &[(String, String)]) -> bool {
    for (_, field_type) in fields {
        if field_type_contains(field_type, name) {
            return true;
        }
    }
    false
}

/// Check if a type annotation string references a given type name.
fn field_type_contains(field_type: &str, type_name: &str) -> bool {
    // Check for exact match or as part of generic: List<Foo>, Option<Foo>, etc.
    field_type == type_name
        || field_type.contains(&format!("<{}", type_name))
        || field_type.contains(&format!("{}>", type_name))
        || field_type.contains(&format!(", {}", type_name))
        || field_type.contains(&format!("{},", type_name))
}

fn emit_sum_type(name: &str, variants: &[TypeVariant]) -> String {
    let mut lines = Vec::new();
    let is_recursive = is_recursive_type(name, variants);

    lines.push(format!("inductive {} where", name));
    for v in variants {
        let lean_name = to_lower_first(&v.name);
        if v.fields.is_empty() {
            lines.push(format!("  | {}", lean_name));
        } else {
            let field_types: Vec<String> = v
                .fields
                .iter()
                .map(|f| type_annotation_to_lean(f))
                .collect();
            // Lean inductive: fields as positional args after colon
            let fields_str = field_types
                .iter()
                .map(|t| format!("({} : {})", "_", t))
                .collect::<Vec<_>>()
                .join(" ");
            lines.push(format!("  | {} {}", lean_name, fields_str));
        }
    }

    if is_recursive {
        // #14: Recursive types cannot derive DecidableEq automatically
        lines.push("  deriving Repr, BEq, Inhabited".to_string());
    } else {
        lines.push("  deriving Repr, BEq, Inhabited, DecidableEq".to_string());
    }
    lines.join("\n")
}

fn emit_product_type(name: &str, fields: &[(String, String)]) -> String {
    let mut lines = Vec::new();
    let is_recursive = is_recursive_product(name, fields);

    lines.push(format!("structure {} where", name));
    for (field_name, field_type) in fields {
        lines.push(format!(
            "  {} : {}",
            aver_name_to_lean(field_name),
            type_annotation_to_lean(field_type)
        ));
    }

    if is_recursive {
        lines.push("  deriving Repr, BEq, Inhabited".to_string());
    } else {
        lines.push("  deriving Repr, BEq, Inhabited, DecidableEq".to_string());
    }
    lines.join("\n")
}

/// Check if a type definition is self-referencing (#18).
pub fn is_recursive_type_def(td: &TypeDef) -> bool {
    match td {
        TypeDef::Sum { name, variants, .. } => is_recursive_type(name, variants),
        TypeDef::Product { name, fields, .. } => is_recursive_product(name, fields),
    }
}

/// Get the name of a type definition.
pub fn type_def_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } => name,
        TypeDef::Product { name, .. } => name,
    }
}

/// Emit unsafe DecidableEq instance for a recursive type (#18).
/// Same pattern as Float DecidableEq in prelude.
pub fn emit_recursive_decidable_eq(name: &str) -> String {
    let mut lines = Vec::new();
    lines.push(format!(
        "private unsafe def {}.unsafeDecEq (a b : {}) : Decidable (a = b) :=",
        name, name
    ));
    lines.push("  if a == b then isTrue (unsafeCast ()) else isFalse (unsafeCast ())".to_string());
    lines.push(format!("@[implemented_by {}.unsafeDecEq]", name));
    lines.push(format!(
        "private opaque {}.compDecEq (a b : {}) : Decidable (a = b)",
        name, name
    ));
    lines.push(format!(
        "instance : DecidableEq {} := {}.compDecEq",
        name, name
    ));
    lines.join("\n")
}

const STRING_POS_FUEL_VAR: &str = "fuel'";
const PROOF_FUEL_EXHAUSTED: &str = "panic! \"Aver proof fuel exhausted\"";

fn fuel_helper_name(name: &str) -> String {
    format!("{}__fuel", aver_name_to_lean(name))
}

fn emit_fn_param_names(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, _)| aver_name_to_lean(name))
        .collect::<Vec<_>>()
        .join(" ")
}

fn indent_lines(block: &str, prefix: &str) -> Vec<String> {
    block
        .lines()
        .map(|line| format!("{prefix}{line}"))
        .collect()
}

fn emit_doc_comment(desc: &Option<String>) -> Vec<String> {
    desc.as_ref()
        .map(|text| vec![format!("/-- {} -/", text)])
        .unwrap_or_default()
}

fn ret_type_or_unit(fd: &FnDef) -> String {
    if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    }
}

fn emit_fuel_helper_def(
    helper_name: &str,
    params: &str,
    ret_type: &str,
    body: &str,
    outer_indent: &str,
) -> Vec<String> {
    let branch_indent = format!("{outer_indent}    ");
    [
        vec![format!(
            "{outer_indent}def {} (fuel : Nat) {} : {} :=",
            helper_name, params, ret_type
        )],
        vec![format!("{outer_indent}  match fuel with")],
        vec![format!("{outer_indent}  | 0 => {}", PROOF_FUEL_EXHAUSTED)],
        vec![format!("{outer_indent}  | {} + 1 =>", STRING_POS_FUEL_VAR)],
        indent_lines(body, &branch_indent),
    ]
    .into_iter()
    .flatten()
    .collect()
}

fn emit_string_pos_wrapper(fd: &FnDef, helper_name: &str, rank_budget: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let (s_name, _) = &fd.params[0];
    let (pos_name, _) = &fd.params[1];
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} (averStringPosFuel {} {} {}) {}",
            helper_name,
            aver_name_to_lean(s_name),
            aver_name_to_lean(pos_name),
            rank_budget,
            arg_names
        ),
    ]
}

fn emit_int_countdown_wrapper(fd: &FnDef, helper_name: &str, param_index: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!("  {} ((Int.natAbs {}) + 1) {}", helper_name, metric_name, arg_names),
    ]
}

fn rewrite_recursive_calls_expr(expr: &Expr, targets: &HashSet<String>, fuel_var: &str) -> Expr {
    match expr {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => expr.clone(),
        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(rewrite_recursive_calls_expr(obj, targets, fuel_var)),
            field.clone(),
        ),
        Expr::FnCall(callee, args) => {
            let rewritten_args: Vec<Expr> = args
                .iter()
                .map(|arg| rewrite_recursive_calls_expr(arg, targets, fuel_var))
                .collect();
            if let Some(name) = expr_to_dotted_name(callee)
                && targets.contains(&name)
            {
                let mut call_args = Vec::with_capacity(rewritten_args.len() + 1);
                call_args.push(Expr::Ident(fuel_var.to_string()));
                call_args.extend(rewritten_args);
                Expr::FnCall(Box::new(Expr::Ident(fuel_helper_name(&name))), call_args)
            } else {
                Expr::FnCall(
                    Box::new(rewrite_recursive_calls_expr(callee, targets, fuel_var)),
                    rewritten_args,
                )
            }
        }
        Expr::BinOp(op, left, right) => Expr::BinOp(
            op.clone(),
            Box::new(rewrite_recursive_calls_expr(left, targets, fuel_var)),
            Box::new(rewrite_recursive_calls_expr(right, targets, fuel_var)),
        ),
        Expr::Match {
            subject,
            arms,
            line,
        } => Expr::Match {
            subject: Box::new(rewrite_recursive_calls_expr(subject, targets, fuel_var)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rewrite_recursive_calls_expr(&arm.body, targets, fuel_var)),
                })
                .collect(),
            line: *line,
        },
        Expr::Constructor(name, arg) => Expr::Constructor(
            name.clone(),
            arg.as_ref()
                .map(|inner| Box::new(rewrite_recursive_calls_expr(inner, targets, fuel_var))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(rewrite_recursive_calls_expr(
            inner, targets, fuel_var,
        ))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(_) => part.clone(),
                    StrPart::Parsed(inner) => StrPart::Parsed(Box::new(
                        rewrite_recursive_calls_expr(inner, targets, fuel_var),
                    )),
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| rewrite_recursive_calls_expr(item, targets, fuel_var))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| rewrite_recursive_calls_expr(item, targets, fuel_var))
                .collect(),
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(k, v)| {
                    (
                        rewrite_recursive_calls_expr(k, targets, fuel_var),
                        rewrite_recursive_calls_expr(v, targets, fuel_var),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        rewrite_recursive_calls_expr(value, targets, fuel_var),
                    )
                })
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(rewrite_recursive_calls_expr(base, targets, fuel_var)),
            updates: updates
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        rewrite_recursive_calls_expr(value, targets, fuel_var),
                    )
                })
                .collect(),
        },
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            let rewritten_args: Vec<Expr> = args
                .iter()
                .map(|arg| rewrite_recursive_calls_expr(arg, targets, fuel_var))
                .collect();
            if targets.contains(target) {
                let mut call_args = Vec::with_capacity(rewritten_args.len() + 1);
                call_args.push(Expr::Ident(fuel_var.to_string()));
                call_args.extend(rewritten_args);
                Expr::FnCall(Box::new(Expr::Ident(fuel_helper_name(target))), call_args)
            } else {
                Expr::TailCall(Box::new((target.clone(), rewritten_args)))
            }
        }
    }
}

fn rewrite_recursive_calls_body(
    body: &FnBody,
    targets: &HashSet<String>,
    fuel_var: &str,
) -> FnBody {
    FnBody::Block(
        body.stmts()
            .iter()
            .map(|stmt| match stmt {
                Stmt::Binding(name, ty, expr) => Stmt::Binding(
                    name.clone(),
                    ty.clone(),
                    rewrite_recursive_calls_expr(expr, targets, fuel_var),
                ),
                Stmt::Expr(expr) => {
                    Stmt::Expr(rewrite_recursive_calls_expr(expr, targets, fuel_var))
                }
            })
            .collect(),
    )
}

fn emit_fuelized_string_pos_fn(fd: &FnDef, ctx: &CodegenContext) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = emit_fn_body(&rewritten, ctx);

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_string_pos_wrapper(fd, &helper_name, 1),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_fuelized_int_countdown_fn(fd: &FnDef, ctx: &CodegenContext, param_index: usize) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = emit_fn_body(&rewritten, ctx);

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_countdown_wrapper(fd, &helper_name, param_index),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_fuelized_mutual_string_pos_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
    plans: &HashMap<String, RecursionPlan>,
) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let max_rank = fns
        .iter()
        .filter_map(|fd| match plans.get(&fd.name) {
            Some(RecursionPlan::MutualStringPosAdvance { rank }) => Some(*rank),
            _ => None,
        })
        .max()
        .unwrap_or(1);

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body(&rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_string_pos_wrapper(fd, &helper_name, max_rank);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

fn emit_fuelized_mutual_int_countdown_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body(&rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_int_countdown_wrapper(fd, &helper_name, 0);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

/// Check if a function is pure (no effects) and not main.
pub fn is_pure_fn(fd: &FnDef) -> bool {
    fd.effects.is_empty() && fd.name != "main"
}

/// Emit a Lean 4 function definition from an Aver FnDef.
/// Returns `None` if the function should be skipped (effectful, main).
pub fn emit_fn_def(
    fd: &FnDef,
    recursive_fns: &HashSet<String>,
    ctx: &CodegenContext,
) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    let mut lines = Vec::new();

    // Doc comment from description
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", desc));
    }

    let is_recursive = recursive_fns.contains(&fd.name);
    let fn_name = aver_name_to_lean(&fd.name);

    // Parameters
    let params = emit_fn_params(&fd.params);

    // Return type
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };

    // partial for recursive functions
    let prefix = if is_recursive { "partial " } else { "" };

    lines.push(format!(
        "{}def {} {} : {} :=",
        prefix, fn_name, params, ret_type
    ));
    lines.push(emit_fn_body(&fd.body, ctx));

    Some(lines.join("\n"))
}

/// Proof-mode function emission:
/// recursive functions use explicit `termination_by` based on analyzed recursion plan.
pub fn emit_fn_def_proof(
    fd: &FnDef,
    recursion_plan: Option<RecursionPlan>,
    ctx: &CodegenContext,
) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    if let Some(RecursionPlan::IntCountdown { param_index }) = recursion_plan {
        return Some(emit_fuelized_int_countdown_fn(fd, ctx, param_index));
    }

    if matches!(recursion_plan, Some(RecursionPlan::StringPosAdvance)) {
        return Some(emit_fuelized_string_pos_fn(fd, ctx));
    }

    let mut lines = Vec::new();
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", desc));
    }

    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };
    lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
    lines.push(emit_fn_body(&fd.body, ctx));

    if let Some(plan) = recursion_plan {
        match plan {
            RecursionPlan::IntCountdown { .. } => {}
            RecursionPlan::MutualIntCountdown => {
                let Some((param_name, _)) = fd.params.first() else {
                    return Some(lines.join("\n"));
                };
                let lean_param = aver_name_to_lean(param_name);
                lines.push(format!("termination_by Int.natAbs {}", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  omega".to_string());
            }
            RecursionPlan::ListStructural => {
                let Some((param_name, _)) = fd.params.first() else {
                    return Some(lines.join("\n"));
                };
                let lean_param = aver_name_to_lean(param_name);
                lines.push(format!("termination_by {}.length", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  decreasing_tactic".to_string());
            }
            RecursionPlan::StringPosAdvance => {}
            RecursionPlan::MutualStringPosAdvance { .. }
            | RecursionPlan::MutualSizeOfRanked { .. } => {}
        }
    }

    Some(lines.join("\n"))
}

fn emit_fn_params(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, type_ann)| {
            let lean_type = type_annotation_to_lean(type_ann);
            let lean_name = aver_name_to_lean(name);
            format!("({} : {})", lean_name, lean_type)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn emit_fn_body(body: &FnBody, ctx: &CodegenContext) -> String {
    let stmts = body.stmts();
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        match stmt {
            Stmt::Binding(_, _, _) => {
                lines.push(format!("  {}", emit_stmt(stmt, ctx)));
            }
            Stmt::Expr(expr) => {
                if is_last {
                    lines.push(format!("  {}", emit_expr(expr, ctx)));
                } else {
                    lines.push(format!("  let _ := {}", emit_expr(expr, ctx)));
                }
            }
        }
    }
    lines.join("\n")
}

/// Emit verify blocks as Lean 4 `example` declarations.
///
/// `native_decide` gives executable proof checks for decidable goals.
/// `sorry` is available as explicit fallback mode.
pub fn emit_verify_block(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    if let VerifyKind::Law(law) = &vb.kind {
        return emit_verify_law_block(vb, law, ctx, verify_mode, case_index_start);
    }

    let mut lines = Vec::new();
    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        let left_str = emit_expr(left, ctx);
        let right_str = emit_expr(right, ctx);
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "example : {} = {} := by native_decide",
                    left_str, right_str
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "example : {} = {} := by sorry",
                    left_str, right_str
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                let theorem_name = format!(
                    "{}_verify_{}",
                    aver_name_to_lean(&vb.fn_name),
                    case_index_start + idx + 1
                );
                lines.push(format!(
                    "theorem {} : {} = {} := by",
                    theorem_name, left_str, right_str
                ));
                lines.push("  sorry".to_string());
            }
        }
    }
    (lines.join("\n"), case_index_start + vb.cases.len())
}

fn emit_verify_law_block(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    let mut lines = Vec::new();
    let fn_name = aver_name_to_lean(&vb.fn_name);
    let law_name = aver_name_to_lean(&law.name);
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs);
    let theorem_base = match &spec_ref {
        Some(spec_ref) => format!("{}_eq_{}", fn_name, aver_name_to_lean(&spec_ref.spec_fn_name)),
        None => format!("{}_law_{}", fn_name, law_name),
    };
    let lhs_template = emit_expr(&law.lhs, ctx);
    let rhs_template = emit_expr(&law.rhs, ctx);
    let quant_params = law
        .givens
        .iter()
        .map(|given| {
            format!(
                "({} : {})",
                aver_name_to_lean(&given.name),
                type_annotation_to_lean(&given.type_name)
            )
        })
        .collect::<Vec<_>>()
        .join(" ");

    match &spec_ref {
        Some(spec_ref) => lines.push(format!(
            "-- verify law {}.spec {} ({} cases)",
            fn_name, spec_ref.spec_fn_name, vb.cases.len()
        )),
        None => lines.push(format!(
            "-- verify law {}.{} ({} cases)",
            fn_name, law_name, vb.cases.len()
        )),
    }
    for given in &law.givens {
        lines.push(format!(
            "-- given {}: {} = {}",
            aver_name_to_lean(&given.name),
            given.type_name,
            law_given_domain_to_lean(&given.domain, ctx)
        ));
    }
    if !quant_params.is_empty() {
        if let Some(auto_proof) = emit_verify_law_forall_auto_proof(vb, law, ctx, verify_mode) {
            lines.push(format!(
                "theorem {} : ∀ {}, {} = {} := by",
                theorem_base, quant_params, lhs_template, rhs_template
            ));
            lines.extend(auto_proof);
        } else if verify_mode == VerifyEmitMode::NativeDecide {
            lines.push(format!(
                "-- universal theorem {} omitted: sampled law shape is not auto-proved yet",
                theorem_base
            ));
        } else {
            lines.push(format!(
                "theorem {} : ∀ {}, {} = {} := by",
                theorem_base, quant_params, lhs_template, rhs_template
            ));
            lines.push(
                "  -- verify law is sampled; universal proof must be provided manually".to_string(),
            );
            lines.push("  sorry".to_string());
        }
    }

    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        let theorem_name = format!("{}_sample_{}", theorem_base, case_index_start + idx + 1);
        let left_str = emit_expr(left, ctx);
        let right_str = emit_expr(right, ctx);
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "theorem {} : {} = {} := by native_decide",
                    theorem_name, left_str, right_str
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "theorem {} : {} = {} := by sorry",
                    theorem_name, left_str, right_str
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                lines.push(format!(
                    "theorem {} : {} = {} := by",
                    theorem_name, left_str, right_str
                ));
                lines.push("  sorry".to_string());
            }
        }
    }
    (lines.join("\n"), case_index_start + vb.cases.len())
}

fn law_given_domain_to_lean(domain: &VerifyGivenDomain, ctx: &CodegenContext) -> String {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => format!("{}..{}", start, end),
        VerifyGivenDomain::Explicit(values) => format!(
            "[{}]",
            values
                .iter()
                .map(|v| emit_expr(v, ctx))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// Emit a decision block as a Lean 4 block comment.
pub fn emit_decision(db: &DecisionBlock) -> String {
    let mut lines = Vec::new();
    lines.push(format!("/- Decision: {}", db.name));
    lines.push(format!("   Date: {}", db.date));
    lines.push(format!("   Reason: {}", db.reason));
    lines.push(format!("   Chosen: {}", db.chosen.as_context_string()));
    if !db.rejected.is_empty() {
        lines.push(format!(
            "   Rejected: {}",
            db.rejected
                .iter()
                .map(|r| r.as_context_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if !db.impacts.is_empty() {
        let impacts = db
            .impacts
            .iter()
            .map(|impact| impact.as_context_string())
            .collect::<Vec<_>>()
            .join(", ");
        lines.push(format!("   Impacts: {}", impacts));
    }
    if let Some(author) = &db.author {
        lines.push(format!("   Author: {}", author));
    }
    lines.push("-/".to_string());
    lines.join("\n")
}

/// Emit mutual recursion group wrapped in `mutual ... end`.
pub fn emit_mutual_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", desc));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        // #17: All functions in mutual blocks need `partial` for termination
        lines.push(format!(
            "  partial def {} {} : {} :=",
            fn_name, params, ret_type
        ));
        let body = emit_fn_body(&fd.body, ctx);
        // Indent body by 2 more spaces
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        lines.push(String::new());
    }
    lines.push("end".to_string());
    lines.join("\n")
}

/// Proof-mode mutual recursion emission with optional group-level termination.
pub fn emit_mutual_group_proof(
    fns: &[&FnDef],
    ctx: &CodegenContext,
    plans: &std::collections::HashMap<String, RecursionPlan>,
) -> String {
    if fns
        .iter()
        .all(|fd| matches!(plans.get(&fd.name), Some(RecursionPlan::MutualIntCountdown)))
    {
        return emit_fuelized_mutual_int_countdown_group(fns, ctx);
    }

    if fns.iter().all(|fd| {
        matches!(
            plans.get(&fd.name),
            Some(RecursionPlan::MutualStringPosAdvance { .. })
        )
    }) {
        return emit_fuelized_mutual_string_pos_group(fns, ctx, plans);
    }

    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", desc));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        let body = emit_fn_body(&fd.body, ctx);
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        match plans.get(&fd.name).copied() {
            Some(RecursionPlan::MutualIntCountdown) => {
                if let Some((first_name, _)) = fd.params.first() {
                    let lean_first = aver_name_to_lean(first_name);
                    lines.push(format!("  termination_by Int.natAbs {}", lean_first));
                    lines.push("  decreasing_by".to_string());
                    lines.push("    omega".to_string());
                }
            }
            Some(RecursionPlan::MutualStringPosAdvance { rank }) => {
                if let Some((s_name, _)) = fd.params.first()
                    && let Some((pos_name, _)) = fd.params.get(1)
                {
                    let lean_s = aver_name_to_lean(s_name);
                    let lean_pos = aver_name_to_lean(pos_name);
                    lines.push(format!(
                        "  termination_by (({}.data.length) - ({}.toNat), {})",
                        lean_s, lean_pos, rank
                    ));
                    lines.push("  decreasing_by".to_string());
                    lines.push("    simp_wf".to_string());
                }
            }
            Some(RecursionPlan::MutualSizeOfRanked {
                rank: _,
                metric_param_index: _,
            }) => {}
            _ => {}
        }
        lines.push(String::new());
    }

    lines.push("end".to_string());
    lines.join("\n")
}

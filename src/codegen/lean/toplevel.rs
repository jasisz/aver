/// Top-level Aver items → Lean 4 items (defs, inductives, structures, examples).
use std::collections::HashSet;

use super::expr::{aver_name_to_lean, emit_expr, emit_stmt};
use super::shared::to_lower_first;
use super::types::type_annotation_to_lean;
use super::{RecursionPlan, VerifyEmitMode};
use crate::ast::*;
use crate::codegen::CodegenContext;

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
        if let Some((param_name, _)) = fd.params.first() {
            let lean_param = aver_name_to_lean(param_name);
            match plan {
                RecursionPlan::IntCountdown | RecursionPlan::MutualIntCountdown => {
                    lines.push(format!("termination_by Int.natAbs {}", lean_param));
                    lines.push("decreasing_by".to_string());
                    lines.push("  simp_wf".to_string());
                }
                RecursionPlan::ListStructural => {
                    lines.push(format!("termination_by {}.length", lean_param));
                    lines.push("decreasing_by".to_string());
                    lines.push("  simp_wf".to_string());
                }
                RecursionPlan::StringPosAdvance => {
                    if let Some((s_name, _)) = fd.params.first() {
                        if let Some((pos_name, _)) = fd.params.get(1) {
                            let lean_s = aver_name_to_lean(s_name);
                            let lean_pos = aver_name_to_lean(pos_name);
                            lines.push(format!(
                                "termination_by Int.natAbs ((Int.ofNat ({}.length)) - {})",
                                lean_s, lean_pos
                            ));
                            lines.push("decreasing_by".to_string());
                            lines.push("  simp_wf".to_string());
                        }
                    }
                }
                RecursionPlan::MutualStringPosAdvance { .. }
                | RecursionPlan::MutualSizeOfRanked { .. } => {}
            }
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
    match body {
        FnBody::Expr(expr) => {
            format!("  {}", emit_expr(expr, ctx))
        }
        FnBody::Block(stmts) => {
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
                            // Non-last expression — emit as let _ :=
                            lines.push(format!("  let _ := {}", emit_expr(expr, ctx)));
                        }
                    }
                }
            }
            lines.join("\n")
        }
    }
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

/// Emit a decision block as a Lean 4 block comment.
pub fn emit_decision(db: &DecisionBlock) -> String {
    let mut lines = Vec::new();
    lines.push(format!("/- Decision: {}", db.name));
    lines.push(format!("   Date: {}", db.date));
    lines.push(format!("   Reason: {}", db.reason));
    lines.push(format!("   Chosen: {}", db.chosen));
    if !db.rejected.is_empty() {
        lines.push(format!("   Rejected: {}", db.rejected.join(", ")));
    }
    if !db.impacts.is_empty() {
        lines.push(format!("   Impacts: {}", db.impacts.join(", ")));
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
        lines.push(String::new());
    }

    let all_mutual_int = fns
        .iter()
        .all(|fd| matches!(plans.get(&fd.name), Some(RecursionPlan::MutualIntCountdown)));
    let all_mutual_string_pos = fns.iter().all(|fd| {
        matches!(
            plans.get(&fd.name),
            Some(RecursionPlan::MutualStringPosAdvance { .. })
        )
    });
    let all_mutual_sizeof = fns.iter().all(|fd| {
        matches!(
            plans.get(&fd.name),
            Some(RecursionPlan::MutualSizeOfRanked { .. })
        )
    });
    if all_mutual_int {
        lines.push("termination_by".to_string());
        for fd in fns {
            if !is_pure_fn(fd) {
                continue;
            }
            let fn_name = aver_name_to_lean(&fd.name);
            let mut lhs_params: Vec<String> = Vec::new();
            let mut first_param = "x".to_string();
            for (idx, (name, _)) in fd.params.iter().enumerate() {
                if idx == 0 {
                    let lean = aver_name_to_lean(name);
                    first_param = lean.clone();
                    lhs_params.push(lean);
                } else {
                    lhs_params.push("_".to_string());
                }
            }
            let lhs = if lhs_params.is_empty() {
                "_".to_string()
            } else {
                lhs_params.join(" ")
            };
            lines.push(format!(
                "  {} {} => Int.natAbs {}",
                fn_name, lhs, first_param
            ));
        }
        lines.push("decreasing_by".to_string());
        lines.push("  simp_wf".to_string());
    } else if all_mutual_string_pos {
        lines.push("termination_by".to_string());
        for fd in fns {
            if !is_pure_fn(fd) {
                continue;
            }
            let fn_name = aver_name_to_lean(&fd.name);
            let mut lhs_params: Vec<String> = Vec::new();
            let mut s_param = "s".to_string();
            let mut pos_param = "pos".to_string();
            for (idx, (name, _)) in fd.params.iter().enumerate() {
                if idx == 0 {
                    s_param = aver_name_to_lean(name);
                    lhs_params.push(s_param.clone());
                } else if idx == 1 {
                    pos_param = aver_name_to_lean(name);
                    lhs_params.push(pos_param.clone());
                } else {
                    lhs_params.push("_".to_string());
                }
            }
            let lhs = if lhs_params.is_empty() {
                "_".to_string()
            } else {
                lhs_params.join(" ")
            };
            let rank = match plans.get(&fd.name) {
                Some(RecursionPlan::MutualStringPosAdvance { rank }) => *rank,
                _ => 0,
            };
            lines.push(format!(
                "  {} {} => (Int.natAbs ((Int.ofNat ({}.length)) - {}), {})",
                fn_name, lhs, s_param, pos_param, rank
            ));
        }
        lines.push("decreasing_by".to_string());
        lines.push("  simp_wf".to_string());
    } else if all_mutual_sizeof {
        lines.push("termination_by".to_string());
        for fd in fns {
            if !is_pure_fn(fd) {
                continue;
            }
            let fn_name = aver_name_to_lean(&fd.name);
            let (metric_idx, rank) = match plans.get(&fd.name) {
                Some(RecursionPlan::MutualSizeOfRanked {
                    rank,
                    metric_param_index,
                }) => (*metric_param_index, *rank),
                _ => (0, 0),
            };
            let mut lhs_params: Vec<String> = Vec::new();
            let mut metric_param = "x".to_string();
            for (idx, (name, _)) in fd.params.iter().enumerate() {
                if idx == metric_idx {
                    let lean = aver_name_to_lean(name);
                    metric_param = lean.clone();
                    lhs_params.push(lean);
                } else {
                    lhs_params.push("_".to_string());
                }
            }
            let lhs = if lhs_params.is_empty() {
                "_".to_string()
            } else {
                lhs_params.join(" ")
            };
            lines.push(format!(
                "  {} {} => (sizeOf {}, {})",
                fn_name, lhs, metric_param, rank
            ));
        }
        lines.push("decreasing_by".to_string());
        lines.push("  simp_wf".to_string());
    }

    lines.push("end".to_string());
    lines.join("\n")
}

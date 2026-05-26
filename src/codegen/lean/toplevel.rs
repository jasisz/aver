/// Top-level Aver items → Lean 4 items (defs, inductives, structures, examples).
use std::collections::{HashMap, HashSet};

use super::expr::{aver_name_to_lean, emit_expr, emit_stmt};
use super::law_auto::{emit_verify_law_forall_auto_proof, emit_verify_law_support_theorems};
use super::recurrence::{
    detect_second_order_int_linear_recurrence, recurrence_nat_helper_name, render_affine_pair_expr,
};
use super::shared::to_lower_first;
use super::types::type_annotation_to_lean;
use super::{VerifyEmitMode, sizeof_measure_param_indices};
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::recursion::{native_aux_name, rewrite_recursive_calls_body};
use crate::verify_law::canonical_spec_ref;

/// Emit a Lean 4 type definition from an Aver TypeDef.
/// Subtype helper-type names that the oracle_subtypes module emits at
/// the top of every artifact. Returned name matches the
/// `<...>InBounds` / `<...>InUnit` / `<...>Nonneg` declarations in
/// `oracle_subtypes::lean_subtypes` — keep these in sync.
fn bounded_oracle_subtype_for(method: &str) -> Option<&'static str> {
    match method {
        "Random.int" => Some("RandomIntInBounds"),
        "Random.float" => Some("RandomFloatInUnit"),
        "Time.unixMs" => Some("TimeUnixMsNonneg"),
        _ => None,
    }
}

pub fn emit_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_in_scope(td, ctx, None)
}

/// Module-scoped emit: `scope` carries the prefix of the module
/// whose typedefs we're currently rendering (or `None` for entry
/// items). Drives [`find_refined_type_scoped`] so a refined record
/// with a bare name resolves to the current module's canonical
/// entry instead of whichever module populated first.
pub fn emit_type_def_in_scope(td: &TypeDef, ctx: &CodegenContext, scope: Option<&str>) -> String {
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields, ctx, scope),
    }
}

/// Check if a sum type is self-referencing (any variant field mentions the type name).
// Recursive-type / field-type predicates moved to `codegen::common` so
// all three backends share a single source of truth. Re-exported below
// as pub(crate) for existing call sites in this backend.
pub(crate) use crate::codegen::common::{
    is_pure_fn, is_recursive_product, is_recursive_sum as is_recursive_type, is_recursive_type_def,
    type_def_name,
};

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

fn emit_product_type(
    name: &str,
    fields: &[(String, String)],
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    // Refinement-via-opaque records emit as Lean `Subtype` only
    // when the carrier is `Int`. Float-carrier records (NonNegFloat,
    // Discount, …) stay as plain `structure`, because Lean's `Float`
    // model doesn't admit universal arithmetic laws (IEEE 754: `NaN
    // ≠ NaN`, `+` not commutative across infinities), so the lift
    // would just produce uniwersalne theorems we can't prove. The
    // existing sample-based path covers them: domain values come
    // from `given a: Float = […]`, and proofs are sample-by-sample
    // via native_decide.
    if let Some(decl) = crate::codegen::common::find_refined_type_scoped(ctx, name, scope)
        && decl.carrier_type == "Int"
    {
        let carrier_ty = type_annotation_to_lean(&decl.carrier_type);
        let param = aver_name_to_lean(&decl.predicate_param);
        let predicate = super::expr::emit_expr(&decl.invariant.expr, ctx);
        return format!("abbrev {name} := {{ {param} : {carrier_ty} // {predicate} }}");
    }

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

fn measure_fn_name(type_name: &str) -> String {
    format!("averMeasure{}", type_name)
}

fn measure_list_fn_name(type_name: &str) -> String {
    format!("{}List", measure_fn_name(type_name))
}

fn measure_entries_fn_name(type_name: &str, key_type: &str) -> String {
    let key_suffix: String = key_type
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect();
    format!("{}Entries_{}", measure_fn_name(type_name), key_suffix)
}

fn split_top_level(s: &str, delim: char) -> Vec<String> {
    crate::codegen::common::split_type_params(s, delim)
}

fn unwrap_generic<'a>(type_name: &'a str, prefix: &str) -> Option<&'a str> {
    type_name
        .strip_prefix(prefix)
        .and_then(|rest| rest.strip_suffix('>'))
}

fn type_measure_expr(
    type_name: &str,
    value_expr: &str,
    recursive_types: &HashSet<String>,
    self_type: Option<&str>,
) -> Option<String> {
    let trimmed = type_name.trim();
    if recursive_types.contains(trimmed) {
        return Some(format!("{} {}", measure_fn_name(trimmed), value_expr));
    }

    if let Some(inner) = unwrap_generic(trimmed, "List<") {
        if recursive_types.contains(inner.trim()) {
            return Some(format!(
                "{} {}",
                measure_list_fn_name(inner.trim()),
                value_expr
            ));
        }
        let item_measure = type_measure_expr(inner, "item", recursive_types, self_type)
            .unwrap_or_else(|| "1".to_string());
        return Some(format!(
            "AverMeasure.list (fun item => {}) {}",
            item_measure, value_expr
        ));
    }

    if let Some(inner) = unwrap_generic(trimmed, "Option<") {
        if self_type == Some(inner.trim()) {
            return Some(format!(
                "(match {} with | .none => 1 | .some item => {} item + 1)",
                value_expr,
                measure_fn_name(inner.trim())
            ));
        }
        let item_measure = type_measure_expr(inner, "item", recursive_types, self_type)
            .unwrap_or_else(|| "1".to_string());
        return Some(format!(
            "AverMeasure.option (fun item => {}) {}",
            item_measure, value_expr
        ));
    }

    if let Some(inner) = unwrap_generic(trimmed, "Map<") {
        let args = split_top_level(inner, ',');
        if args.len() == 2 {
            let key_type = args[0].trim();
            let value_type = args[1].trim();
            if recursive_types.contains(value_type) {
                return Some(format!(
                    "{} (AverMap.entries {})",
                    measure_entries_fn_name(value_type, key_type),
                    value_expr
                ));
            }
            let key_measure = type_measure_expr(key_type, "entry.1", recursive_types, self_type);
            let value_measure =
                type_measure_expr(value_type, "entry.2", recursive_types, self_type);
            let entry_measure = match (key_measure, value_measure) {
                (Some(k), Some(v)) => format!("({k}) + ({v}) + 1"),
                (Some(k), None) => format!("({k}) + 1"),
                (None, Some(v)) => format!("({v}) + 1"),
                (None, None) => "1".to_string(),
            };
            return Some(format!(
                "AverMeasure.list (fun entry => {}) (AverMap.entries {})",
                entry_measure, value_expr
            ));
        }
    }

    if let Some(inner) = unwrap_generic(trimmed, "Result<") {
        let args = split_top_level(inner, ',');
        if args.len() == 2 {
            let ok_measure = type_measure_expr(&args[0], "okVal", recursive_types, self_type)
                .unwrap_or_else(|| "1".to_string());
            let err_measure = type_measure_expr(&args[1], "errVal", recursive_types, self_type)
                .unwrap_or_else(|| "1".to_string());
            return Some(format!(
                "AverMeasure.except (fun errVal => {}) (fun okVal => {}) {}",
                err_measure, ok_measure, value_expr
            ));
        }
    }

    if trimmed.starts_with('(') && trimmed.ends_with(')') {
        let inner = &trimmed[1..trimmed.len() - 1];
        let parts = split_top_level(inner, ',');
        if !parts.is_empty() {
            let measures: Vec<String> = parts
                .iter()
                .enumerate()
                .filter_map(|(idx, part)| {
                    type_measure_expr(
                        part,
                        &format!("{}.{}", value_expr, idx + 1),
                        recursive_types,
                        self_type,
                    )
                })
                .collect();
            if !measures.is_empty() {
                return Some(format!("({}) + 1", measures.join(" + ")));
            }
        }
    }

    None
}

fn recursive_map_key_types(type_refs: &[String], value_type: &str) -> Vec<String> {
    let mut key_types = Vec::new();
    for type_ref in type_refs {
        let Some(inner) = unwrap_generic(type_ref.trim(), "Map<") else {
            continue;
        };
        let args = split_top_level(inner, ',');
        if args.len() == 2 && args[1].trim() == value_type {
            let key_type = args[0].trim().to_string();
            if !key_types.contains(&key_type) {
                key_types.push(key_type);
            }
        }
    }
    key_types
}

fn emit_recursive_sum_measure(
    name: &str,
    variants: &[TypeVariant],
    recursive_types: &HashSet<String>,
) -> String {
    let mut lines = vec!["mutual".to_string()];
    lines.push(format!(
        "  def {} (value : {}) : Nat :=",
        measure_fn_name(name),
        name
    ));
    lines.push("    match value with".to_string());
    for variant in variants {
        let ctor = to_lower_first(&variant.name);
        if variant.fields.is_empty() {
            lines.push(format!("    | .{} => 1", ctor));
            continue;
        }

        let binders: Vec<String> = (0..variant.fields.len())
            .map(|idx| format!("x{idx}"))
            .collect();
        let field_measures: Vec<String> = variant
            .fields
            .iter()
            .zip(binders.iter())
            .filter_map(|(field_ty, binder)| {
                type_measure_expr(field_ty, binder, recursive_types, Some(name))
            })
            .collect();
        if field_measures.is_empty() {
            lines.push(format!("    | .{} {} => 1", ctor, binders.join(" ")));
        } else {
            lines.push(format!(
                "    | .{} {} => ({}) + 1",
                ctor,
                binders.join(" "),
                field_measures.join(" + ")
            ));
        }
    }
    lines.push(format!(
        "  def {} (items : List {}) : Nat :=",
        measure_list_fn_name(name),
        name
    ));
    lines.push("    match items with".to_string());
    lines.push("    | [] => 1".to_string());
    lines.push(format!(
        "    | head :: tail => {} head + {} tail + 1",
        measure_fn_name(name),
        measure_list_fn_name(name)
    ));
    let field_types: Vec<String> = variants
        .iter()
        .flat_map(|variant| variant.fields.iter().cloned())
        .collect();
    for key_type in recursive_map_key_types(&field_types, name) {
        lines.push(format!(
            "  def {} (items : List ({} × {})) : Nat :=",
            measure_entries_fn_name(name, &key_type),
            key_type,
            name
        ));
        lines.push("    match items with".to_string());
        lines.push("    | [] => 1".to_string());
        lines.push(format!(
            "    | (_, value) :: tail => {} value + {} tail + 1",
            measure_fn_name(name),
            measure_entries_fn_name(name, &key_type)
        ));
    }
    lines.push("end".to_string());
    lines.join("\n")
}

fn emit_recursive_product_measure(
    name: &str,
    fields: &[(String, String)],
    recursive_types: &HashSet<String>,
) -> String {
    let field_measures: Vec<String> = fields
        .iter()
        .filter_map(|(field_name, field_ty)| {
            type_measure_expr(
                field_ty,
                &format!("value.{}", aver_name_to_lean(field_name)),
                recursive_types,
                Some(name),
            )
        })
        .collect();
    let body = if field_measures.is_empty() {
        "1".to_string()
    } else {
        format!("({}) + 1", field_measures.join(" + "))
    };
    let mut lines = vec![
        "mutual".to_string(),
        format!(
            "  def {} (value : {}) : Nat :=",
            measure_fn_name(name),
            name
        ),
        format!("    {}", body),
        format!(
            "  def {} (items : List {}) : Nat :=",
            measure_list_fn_name(name),
            name
        ),
        "    match items with".to_string(),
        "    | [] => 1".to_string(),
        format!(
            "    | head :: tail => {} head + {} tail + 1",
            measure_fn_name(name),
            measure_list_fn_name(name)
        ),
    ];
    let field_types: Vec<String> = fields.iter().map(|(_, ty)| ty.clone()).collect();
    for key_type in recursive_map_key_types(&field_types, name) {
        lines.push(format!(
            "  def {} (items : List ({} × {})) : Nat :=",
            measure_entries_fn_name(name, &key_type),
            key_type,
            name
        ));
        lines.push("    match items with".to_string());
        lines.push("    | [] => 1".to_string());
        lines.push(format!(
            "    | (_, value) :: tail => {} value + {} tail + 1",
            measure_fn_name(name),
            measure_entries_fn_name(name, &key_type)
        ));
    }
    lines.push("end".to_string());
    lines.join("\n")
}

pub fn emit_recursive_measure(td: &TypeDef, recursive_types: &HashSet<String>) -> Option<String> {
    match td {
        TypeDef::Sum { name, variants, .. } if is_recursive_type(name, variants) => {
            Some(emit_recursive_sum_measure(name, variants, recursive_types))
        }
        TypeDef::Product { name, fields, .. } if is_recursive_product(name, fields) => Some(
            emit_recursive_product_measure(name, fields, recursive_types),
        ),
        _ => None,
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
    // Use the shared helper so the name matches what the shared AST
    // rewrite emits into `Expr::Ident(...)` call sites. The `__fuel`
    // suffix keeps the result a plain ASCII identifier regardless of
    // the source name, so no Lean-specific escaping is needed.
    crate::codegen::recursion::fuel_helper_name(name)
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
        format!(
            "  {} ((Int.natAbs {}) + 1) {}",
            helper_name, metric_name, arg_names
        ),
    ]
}

fn emit_nat_linear_recurrence_fn(
    fd: &FnDef,
    shape: &super::recurrence::SecondOrderIntLinearRecurrenceShape,
    ctx: &CodegenContext,
) -> String {
    let fn_name = aver_name_to_lean(&fd.name);
    let nat_helper_name = recurrence_nat_helper_name(&fd.name);
    let lean_param = aver_name_to_lean(&shape.param_name);
    let ret_type = ret_type_or_unit(fd);
    let nat_step = render_affine_pair_expr(
        shape.recurrence,
        &format!("{nat_helper_name} n"),
        &format!("{nat_helper_name} (n + 1)"),
    );

    [
        emit_doc_comment(&fd.desc),
        vec![
            format!("private def {} : Nat -> {}", nat_helper_name, ret_type),
            format!("  | 0 => {}", emit_expr(&shape.base0, ctx)),
            format!("  | 1 => {}", emit_expr(&shape.base1, ctx)),
            format!("  | n + 2 => {}", nat_step),
            String::new(),
            format!("def {} ({} : Int) : {} :=", fn_name, lean_param, ret_type),
            format!(
                "  if {} < 0 then {} else {} {}.toNat",
                lean_param,
                emit_expr(&shape.negative_branch, ctx),
                nat_helper_name,
                lean_param
            ),
        ],
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_sizeof_measure_expr(fd: &FnDef, recursive_types: &HashSet<String>) -> Option<String> {
    let measure_terms: Vec<String> = sizeof_measure_param_indices(fd)
        .into_iter()
        .filter_map(|idx| {
            fd.params.get(idx).and_then(|(name, type_name)| {
                type_measure_expr(type_name, &aver_name_to_lean(name), recursive_types, None)
            })
        })
        .collect();

    (!measure_terms.is_empty()).then(|| measure_terms.join(" + "))
}

fn emit_mutual_sizeof_wrapper(
    fd: &FnDef,
    helper_name: &str,
    rank_budget: usize,
    recursive_types: &HashSet<String>,
) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let fuel_expr = emit_sizeof_measure_expr(fd, recursive_types)
        .map(|measure| format!("(({}) + 1) * {}", measure, rank_budget))
        .unwrap_or_else(|| rank_budget.to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!("  {} ({}) {}", helper_name, fuel_expr, arg_names),
    ]
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
    let body = emit_fn_body_for(fd, &rewritten, ctx);

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

fn strip_match_eq_binders(body: String) -> String {
    body.lines()
        .map(|line| {
            let trimmed = line.trim_start();
            let indent_len = line.len() - trimmed.len();
            let indent = &line[..indent_len];
            let Some(rest) = trimmed.strip_prefix("match h_") else {
                return line.to_string();
            };
            let Some(colon_idx) = rest.find(" : ") else {
                return line.to_string();
            };
            format!("{indent}match {}", &rest[colon_idx + 3..])
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Native `IntCountdown` emission for closed-world fns with the canonical
/// `match p { 0 -> BASE; _ -> rec(p-1, ...) }` shape. Splits the fn into:
///
/// - `<name>__aux` — the real recursion carrying an explicit `(h : p ≥ 0)`
///   precondition. Recursive callsites in its body are rewritten to call
///   `<name>__aux` instead of `<name>` with an extra `(by omega)` proof
///   obligation appended (synthesized via the
///   `OMEGA_PROOF_SENTINEL` ident — see `lean::expr::emit_expr`).
/// - `<name>` — the public wrapper preserving the original Aver signature.
///   Dispatches on `p ≥ 0` to the aux; the `p < 0` branch returns `BASE`
///   (the source's `0` arm). That falls outside the Aver well-formed
///   domain for the issue-84 fibonacci-style targets, but keeping the aux
///   private and total avoids forcing every call site (verify samples,
///   peer fn bodies) to thread proof obligations.
///
/// Lean accepts this because the aux's `termination_by p.natAbs` together
/// with `(h : p ≥ 0)` + the `_` arm's implicit `p ≠ 0` lets `omega`
/// discharge `(p - 1).natAbs < p.natAbs` mechanically.
fn emit_native_guarded_int_countdown_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    base_arm_literal: i64,
    base_arm_body: &Spanned<Expr>,
    wildcard_arm_body: &Spanned<Expr>,
    precondition: &[Spanned<Expr>],
) -> String {
    let aux_name = native_aux_name(&fd.name);
    let main_name = aver_name_to_lean(&fd.name);
    let lean_aux_name = aver_name_to_lean(&aux_name);
    let Some((param_name, _)) = fd.params.get(param_index) else {
        return emit_fuelized_int_countdown_fn(fd, ctx, param_index);
    };
    let lean_pname = aver_name_to_lean(param_name);

    // Precondition: AND of caller-derived predicates, or `(p ≥ 0)`
    // when the artifact has no single external caller (free-standing
    // fns / test fixtures). Same `Spanned<Expr>`-as-predicate path
    // opaque types use, so `emit_expr` is the single emitter — no
    // parallel infrastructure.
    let precond_lean = if precondition.is_empty() {
        format!("{} ≥ 0", lean_pname)
    } else {
        precondition
            .iter()
            .map(|p| format!("({})", emit_expr(p, ctx)))
            .collect::<Vec<_>>()
            .join(" ∧ ")
    };

    let aux_params = format!("{} (h_dom : {})", emit_fn_params(&fd.params), precond_lean);
    let main_params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);

    // Emit `if h_zero : n = LITERAL then BASE else REC` rather than
    // `match n with | LITERAL => ... | _ => ...`. The dependent `if h :
    // c then ... else` form puts `h : c` / `h : ¬c` in scope for the
    // corresponding branch, which `omega` needs to discharge `(n - 1)
    // ≥ 0` and `(n - 1).natAbs < n.natAbs` at the recursive callsite +
    // termination check. Plain `match` would leave the case-split
    // implicit (only an unnamed `casesOn` motive carries it) and
    // `omega` can't see it.
    let rewritten_wc = crate::codegen::recursion::rewrite_native_guarded_calls_expr(
        wildcard_arm_body,
        &fd.name,
        &aux_name,
    );
    let base_str = emit_expr(base_arm_body, ctx);
    let rec_str = emit_expr(&rewritten_wc, ctx);
    let arg_names = emit_fn_param_names(&fd.params);

    let mut lines = Vec::new();
    lines.extend(emit_doc_comment(&fd.desc));
    lines.push(format!(
        "def {} {} : {} :=",
        lean_aux_name, aux_params, ret_type
    ));
    lines.push(format!(
        "  if h_zero : {} = {} then {}",
        lean_pname, base_arm_literal, base_str
    ));
    lines.push(format!("  else {}", rec_str));
    lines.push(format!("termination_by Int.natAbs {}", lean_pname));
    lines.push("decreasing_by".to_string());
    lines.push("  simp_wf".to_string());
    lines.push("  omega".to_string());
    lines.push(String::new());

    lines.push(format!(
        "def {} {} : {} :=",
        main_name, main_params, ret_type
    ));
    lines.push(format!(
        "  if h_dom : {} then {} {} h_dom",
        precond_lean, lean_aux_name, arg_names
    ));
    lines.push(format!("  else {}", base_str));

    lines.join("\n")
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
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

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

fn emit_fuelized_int_ascending_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    bound_lean: &str,
) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_ascending_wrapper(fd, &helper_name, param_index, bound_lean),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_int_ascending_wrapper(
    fd: &FnDef,
    helper_name: &str,
    param_index: usize,
    bound_lean: &str,
) -> Vec<String> {
    let fn_name = super::expr::aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| super::expr::aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} ((Int.natAbs ({} - {})) + 1) {}",
            helper_name, bound_lean, metric_name, arg_names
        ),
    ]
}

fn emit_fuelized_sizeof_fn(fd: &FnDef, ctx: &CodegenContext) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let recursive_types: HashSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| is_recursive_type_def(td))
        .map(|td| type_def_name(td).to_string())
        .collect();
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = emit_fn_body_for(fd, &rewritten, ctx);

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_mutual_sizeof_wrapper(fd, &helper_name, 1, &recursive_types),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

/// Read the rank component of a `Fuel { Lex { .., rank } }` contract.
/// Returns `None` when the fn has no contract or the contract isn't
/// a Lex shape (non-mutual variant or non-recursive).
fn contract_lex_rank(ctx: &CodegenContext, fd: &FnDef) -> Option<usize> {
    contract_lex_params_rank(ctx, fd).map(|(_, rank)| rank)
}

/// Read both the params Vec and rank of a `Fuel { Lex { params, rank } }`
/// contract. Returns `None` for non-Lex / non-recursive / missing
/// contracts. Used by mutual-SCC dispatchers to distinguish:
///
/// - `MutualIntCountdown`: `params.len() == 1`, rank == 0
/// - `MutualStringPosAdvance`: `params.len() == 2`
/// - `MutualSizeOfRanked`: `params.is_empty()`
fn contract_lex_params_rank<'a>(
    ctx: &'a CodegenContext,
    fd: &FnDef,
) -> Option<(&'a [String], usize)> {
    let contract = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?;
    let crate::ir::RecursionContract::Fuel {
        fuel_metric: crate::ir::FuelMetric::Lex { params, rank },
    } = contract.recursion.as_ref()?
    else {
        return None;
    };
    Some((params.as_slice(), *rank))
}

fn emit_fuelized_mutual_string_pos_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let max_rank = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
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
        let body = emit_fn_body_for(fd, &rewritten, ctx);

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
        let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

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

/// Termination measure for Lean's native `termination_by` clause —
/// `name.length` for List/Vector/String params (Lean's stdlib
/// `List.length` is what `decreasing_tactic` knows how to chase),
/// `sizeOf name` fallback for recursive ADTs. Sum across every
/// sizeOf-relevant param.
fn emit_native_termination_measure(fd: &FnDef) -> Option<String> {
    let indices = crate::codegen::recursion::detect::sizeof_measure_param_indices(fd);
    let mut terms: Vec<String> = Vec::new();
    for idx in indices {
        let (name, ty) = fd.params.get(idx)?;
        let parsed = crate::codegen::common::parse_type_annotation(ty);
        let lean_name = aver_name_to_lean(name);
        match parsed {
            crate::types::Type::List(_) | crate::types::Type::Vector(_) => {
                // `sizeOf` instead of `.length` so the user measure
                // matches what Lean's mutual-block wf elaboration
                // generates internally — `decreasing_tactic` then
                // closes the chain without `simp_wf` scrambling.
                terms.push(format!("sizeOf {lean_name}"));
            }
            // Skip Named ADTs and String: `sizeOf` decrease on a
            // recursive ADT in a multi-arg measure (`step f` from
            // `stepApp f arg`, measure `sizeOf f + sizeOf arg`)
            // needs the strict-positivity fact `sizeOf arg ≥ 1`
            // which omega doesn't get for free. Fall back to fuel
            // for those SCCs — accumulator-pattern guard already
            // filters them too, so the only effect here is a more
            // conservative measure-existence gate.
            _ => return None,
        }
    }
    (!terms.is_empty()).then(|| terms.join(" + "))
}

/// Native termination emission for mutual-recursion SCCs whose
/// every member has a sizeOf measure (List / Vector / String) and a
/// classifier rank — Lean 4 `mutual ... end` block with one
/// `termination_by` per def, lex tuple `(sizeOf_sum, rank)` from
/// `MutualSizeOfRanked`. Mirrors the Dafny native path from #83.
///
/// Returns `None` when:
/// - SCC isn't fully `MutualSizeOfRanked` (caller picks fuel)
/// - Any member has no inferable sizeOf measure
/// - Growing-accumulator pattern detected (tail-rec `[x] + acc`
///   shapes won't decrease the lex tuple)
fn emit_native_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> Option<String> {
    let mut ranks: HashMap<String, usize> = HashMap::new();
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        // MutualSizeOfRanked carries `params: vec![]` + rank>=1; any
        // other Lex shape (single-param mutual int-countdown, two-
        // param string-pos) fails this group's pre-conditions.
        match contract_lex_params_rank(ctx, fd) {
            Some(([], rank)) => {
                ranks.insert(fd.name.clone(), rank);
            }
            _ => return None,
        }
    }
    let mut measures: HashMap<String, String> = HashMap::new();
    for fd in fns {
        let measure = emit_native_termination_measure(fd)?;
        measures.insert(fd.name.clone(), measure);
    }
    if crate::codegen::recursion::detect::scc_has_growing_accumulator(fns) {
        return None;
    }

    let mut lines: Vec<String> = vec!["mutual".to_string()];
    for fd in fns {
        let measure = measures.get(&fd.name).unwrap();
        let rank = ranks.get(&fd.name).unwrap();
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        let body_ast = lowered
            .as_ref()
            .map(|l| l.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        let body = emit_fn_body_for(body_fn, body_ast, ctx);

        lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        for body_line in body.lines() {
            lines.push(format!("  {body_line}"));
        }
        lines.push(format!("  termination_by ({measure}, {rank})"));
        // Robust tactic chain — `decreasing_tactic` alone bottoms out
        // on simple shapes (BigInt) but Lean elaborator on multi-arg
        // mutual SCCs sometimes needs `simp_wf` to unfold sizeOf
        // before omega can close the arithmetic on lengths.
        lines.push(
            "  decreasing_by all_goals (first | decreasing_tactic | (simp_wf; (try simp_all); first | omega | (constructor <;> first | rfl | omega)))"
                .to_string(),
        );
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

fn emit_fuelized_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let recursive_types: HashSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| is_recursive_type_def(td))
        .map(|td| type_def_name(td).to_string())
        .collect();
    let rank_budget = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
        .max()
        .unwrap_or(1)
        + 1;

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body_for(fd, &rewritten, ctx);

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
            let mut lines =
                emit_mutual_sizeof_wrapper(fd, &helper_name, rank_budget, &recursive_types);
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

    // Parameters — lifted fn keeps the plain function type for oracle
    // bindings; the subtype constraint is enforced at the lemma level
    // (`∀ rng : RandomIntInBounds, ...`) where it bites the universal
    // claim. Threading the subtype through the operational signature
    // would force every sample binding (`theorem ..._sample_1`) to
    // wrap concrete stubs in `⟨stub, by sorry⟩`, since most user
    // stubs (e.g. `counterStub : fn p n min max := n + min`) only
    // satisfy the bound at specific `(min, max)` pairs and `decide`
    // can't discharge the `∀ min max` quantifier. Sound for the
    // universal lemma, executable for the concrete sample — that's
    // the trade.
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
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    Some(lines.join("\n"))
}

/// Proof-mode function emission. Reads the contract decision from
/// `ctx.proof_ir.fn_contracts` and dispatches to the matching emit fn
/// (native guarded, fuel-encoded, pair-state Nat worker, etc.). Falls
/// back to plain `def` emission when no contract is present (non-
/// recursive fn).
pub fn emit_fn_def_proof(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    // LinearRecurrence2 — dedicated `RecursionContract::LinearRecurrence2`
    // marker. Backend still calls `detect_second_order_int_linear_
    // recurrence` to extract base cases + coefficients; the contract
    // just signals "this fn lowers as pair-state Nat worker, not fuel".
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::LinearRecurrence2)
        )
        && let Some(shape) = detect_second_order_int_linear_recurrence(fd)
    {
        return Some(emit_nat_linear_recurrence_fn(fd, &shape, ctx));
    }

    // IntCountdown now reads through ProofIR's `Fuel { NatAbsPlusOne }`
    // contract. Fuel encoding stays — native `termination_by n.natAbs`
    // would require `(n - 1).natAbs < n.natAbs` which only holds for
    // `n > 0`; Aver bodies don't always clamp to non-negative before
    // recursing (fibTR sans-guard relies on its caller). Fuel
    // sidesteps the issue.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::NatAbsPlusOne { param },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        return Some(emit_fuelized_int_countdown_fn(fd, ctx, param_index));
    }

    // IntCountdownGuarded now reads through ProofIR — the lowerer
    // populates `ctx.proof_ir.fn_contracts` with a `Native` contract
    // whose `precondition` + `body` carry everything the emit needs.
    // Other RecursionPlan variants still flow through `recursion_plan`
    // directly; Step 7+ migrates them one shape at a time.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Native {
            precondition,
            measure: crate::ir::Measure::NatAbsInt { param },
            body,
            ..
        }) = contract.recursion.as_ref()
    {
        // Measure binds the countdown param by name; map back to the
        // arg-position index the emit fn expects. Falls through if the
        // param somehow vanished (shouldn't happen — populator just
        // pulled it from fd.params).
        if let Some(param_index) = fd.params.iter().position(|(n, _)| n == param) {
            let precondition_clauses: Vec<crate::ast::Spanned<crate::ast::Expr>> =
                precondition.iter().map(|p| p.expr.clone()).collect();
            return Some(emit_native_guarded_int_countdown_fn(
                fd,
                ctx,
                param_index,
                body.base_arm_literal,
                &body.base_arm_body,
                &body.wildcard_arm_body,
                &precondition_clauses,
            ));
        }
    }

    // IntAscending reads `Fuel { BoundMinusParamNatAbsPlusOne }`.
    // The bound stays as `Spanned<Expr>` in the contract; backend
    // renders it through `bound_expr_to_lean` here.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne { param, bound },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        let bound_lean = super::bound_expr_to_lean(bound);
        return Some(emit_fuelized_int_ascending_fn(
            fd,
            ctx,
            param_index,
            &bound_lean,
        ));
    }

    // SizeOfStructural — `Fuel { SizeOfPlusOne }`. No params bound;
    // sizeOf walks the whole call frame.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::SizeOfPlusOne,
            })
        )
    {
        return Some(emit_fuelized_sizeof_fn(fd, ctx));
    }

    // StringPosAdvance — `Fuel { StringLenMinusPos { string, pos } }`.
    // Lean's emit reads the params from fd.params directly so the
    // contract just acts as the dispatch signal.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::StringLenMinusPos { .. },
            })
        )
    {
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
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    // termination_by/decreasing_by suffix for the few contract shapes
    // that need explicit Lean termination hints (rest are no-ops —
    // their emit fns already wrote them, or Lean's elaborator infers).
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd) {
        match contract.recursion.as_ref() {
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::Lex { params, rank: 0 },
            }) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param.
                let lean_param = aver_name_to_lean(&params[0]);
                lines.push(format!("termination_by Int.natAbs {}", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  omega".to_string());
            }
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne { param },
            }) => {
                // ListStructural — Lean structural recursion on
                // `<param>.length`. The `+1` framing in the IR is
                // ignored here; Lean's elaborator wants the bare
                // length measure.
                let lean_param = aver_name_to_lean(param);
                lines.push(format!("termination_by {}.length", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  decreasing_tactic".to_string());
            }
            _ => {}
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

fn lower_pure_question_bang_for_emit(fd: &FnDef) -> Option<FnDef> {
    crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
        .ok()
        .flatten()
}

fn expr_uses_error_prop(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(callee, args) => {
            expr_uses_error_prop(callee) || args.iter().any(expr_uses_error_prop)
        }
        Expr::Attr(obj, _) => expr_uses_error_prop(obj),
        Expr::BinOp(_, left, right) => expr_uses_error_prop(left) || expr_uses_error_prop(right),
        Expr::Neg(inner) => expr_uses_error_prop(inner),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|arm| expr_uses_error_prop(&arm.body))
        }
        Expr::Constructor(_, Some(inner)) => expr_uses_error_prop(inner),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            StrPart::Parsed(expr) => expr_uses_error_prop(expr),
            StrPart::Literal(_) => false,
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_uses_error_prop)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_uses_error_prop(key) || expr_uses_error_prop(value)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base)
                || updates.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(expr_uses_error_prop),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

fn body_uses_error_prop(body: &FnBody) -> bool {
    body.stmts().iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_uses_error_prop(expr),
    })
}

fn fn_returns_result(fd: &FnDef) -> bool {
    matches!(
        crate::types::parse_type_str(&fd.return_type),
        crate::types::Type::Result(_, _)
    )
}

fn emit_do_stmt(stmt: &Stmt, ctx: &CodegenContext, is_last: bool) -> String {
    match stmt {
        Stmt::Binding(name, _, expr) if matches!(&expr.node, Expr::ErrorProp(_)) => {
            let Expr::ErrorProp(inner) = &expr.node else {
                unreachable!()
            };
            format!(
                "  let {} <- {}",
                aver_name_to_lean(name),
                emit_expr(inner, ctx)
            )
        }
        Stmt::Binding(name, _, expr) => format!(
            "  let {} := {}",
            aver_name_to_lean(name),
            emit_expr(expr, ctx)
        ),
        Stmt::Expr(expr) if matches!(&expr.node, Expr::ErrorProp(_)) && is_last => {
            let Expr::ErrorProp(inner) = &expr.node else {
                unreachable!()
            };
            format!("  {}", emit_expr(inner, ctx))
        }
        Stmt::Expr(expr) if matches!(&expr.node, Expr::ErrorProp(_)) => {
            let Expr::ErrorProp(inner) = &expr.node else {
                unreachable!()
            };
            format!("  let _ <- {}", emit_expr(inner, ctx))
        }
        Stmt::Expr(expr) if is_last => format!("  {}", emit_expr(expr, ctx)),
        Stmt::Expr(expr) => format!("  let _ := {}", emit_expr(expr, ctx)),
    }
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

fn emit_fn_body_result_do(body: &FnBody, ctx: &CodegenContext) -> String {
    let stmts = body.stmts();
    let mut lines = vec!["  do".to_string()];
    for (i, stmt) in stmts.iter().enumerate() {
        lines.push(emit_do_stmt(stmt, ctx, i == stmts.len() - 1));
    }
    lines.join("\n")
}

fn emit_fn_body_for(fd: &FnDef, body: &FnBody, ctx: &CodegenContext) -> String {
    if fn_returns_result(fd) && body_uses_error_prop(body) {
        emit_fn_body_result_do(body, ctx)
    } else {
        emit_fn_body(body, ctx)
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
    if let VerifyKind::Law(law) = &vb.kind {
        return emit_verify_law_block(vb, law, ctx, verify_mode, case_index_start);
    }

    // Oracle v1: `verify fn trace` cases-form — mix of provable and
    // runtime-only assertions. `.result` projections reduce to
    // `lifted_fn(path, oracle...) = expected` which IS provable;
    // `.trace.length / .event / .contains / .group(..)` project
    // over a runtime-only event buffer that the lifted fn doesn't
    // carry. Emit provable ones as formal examples, comment out the
    // rest with a pointer to docs/oracle.md. Both are checked at
    // runtime via `aver verify` regardless.
    if vb.trace {
        return emit_verify_trace_block_proofs(vb, ctx, verify_mode, case_index_start);
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

/// Oracle v1: emit proof-side assertions for a `verify fn trace`
/// cases-form block. Each case's LHS is inspected — `.result`
/// projections become `example : lifted_fn(root, oracle...) = rhs`,
/// which the auto-proof matcher closes via `simp [fn]` or
/// `native_decide` on concrete samples. Trace-buffer projections
/// (`.trace.length()`, `.event(k)`, `.contains(_)`, `.group(...)`)
/// stay runtime-only; emitted as comments so the proof file
/// still compiles.
fn emit_verify_trace_block_proofs(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    use crate::ast::Expr;
    let mut lines = Vec::new();
    let case_total = vb.cases.len();

    // Build a synthetic VerifyLaw for the rewriter — re-uses
    // `rewrite_effectful_calls_in_law` which wants a law handle to
    // look up given names. Cases-form trace blocks keep their givens
    // on `vb.cases_givens`; the rewriter only reads `law.givens`.
    let synthetic_law = crate::ast::VerifyLaw {
        name: String::new(),
        givens: vb.cases_givens.clone(),
        when: None,
        lhs: vb.cases.first().map(|(l, _)| l.clone()).unwrap_or_else(|| {
            crate::ast::Spanned::new(Expr::Literal(crate::ast::Literal::Unit), vb.line)
        }),
        rhs: vb.cases.first().map(|(_, r)| r.clone()).unwrap_or_else(|| {
            crate::ast::Spanned::new(Expr::Literal(crate::ast::Literal::Unit), vb.line)
        }),
        sample_guards: Vec::new(),
    };

    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        // Shape-detect the LHS. Only `.result` reduces to a formal
        // claim; everything else is runtime-only.
        let result_fn_call = match &left.node {
            Expr::Attr(inner, field) if field == "result" => match &inner.node {
                Expr::FnCall(_, _) => Some((**inner).clone()),
                _ => None,
            },
            _ => None,
        };

        let Some(fn_call) = result_fn_call else {
            let lhs_summary = emit_expr(left, ctx);
            lines.push(format!(
                "-- verify {} trace case {}/{}: `{}` is runtime-only (see docs/oracle.md)",
                vb.fn_name,
                idx + 1,
                case_total,
                lhs_summary,
            ));
            continue;
        };

        // Per-case bindings drive the oracle arg injection — pulls the
        // concrete stub value (e.g. `fairDie`) for each given.
        let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
        let mode = crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
        let lhs_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            &fn_call,
            &synthetic_law,
            |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
            mode.clone(),
        );
        let rhs_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            &synthetic_law,
            |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
            mode,
        );

        let lhs_str = emit_expr(&lhs_rw, ctx);
        let rhs_str = emit_expr(&rhs_rw, ctx);

        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "example : {} = {} := by native_decide",
                    lhs_str, rhs_str
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!("example : {} = {} := by sorry", lhs_str, rhs_str));
            }
            VerifyEmitMode::TheoremSkeleton => {
                let theorem_name = format!(
                    "{}_trace_{}",
                    aver_name_to_lean(&vb.fn_name),
                    case_index_start + idx + 1
                );
                lines.push(format!(
                    "theorem {} : {} = {} := by",
                    theorem_name, lhs_str, rhs_str
                ));
                lines.push("  sorry".to_string());
            }
        }
    }

    (lines.join("\n"), case_index_start + case_total)
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
    // Oracle v1 — issue #127: a verify-trace-law's LHS that projects
    // through `.trace.{event,group,branch,length,contains}` describes
    // the runtime trace buffer, not the lifted fn's return. The lifted
    // Lean form has no `.trace` field — emitting `fn().trace.event 0`
    // as a theorem produces invalid-field-notation errors against the
    // bare return tuple, and the universal `∀ rnd, …` form is the
    // wrong shape anyway (the trace isn't a function of the oracle
    // alone, it's a function of the trace recorder). Emit only a
    // runtime-only marker — matches the cases-form trace block
    // behavior in `emit_verify_trace_block_proofs`. The `aver verify`
    // runtime path still exercises the law under the given stubs.
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        let header = match canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs) {
            Some(spec_ref) => format!(
                "-- verify law {}.spec {}: trace-projection LHS is runtime-only (see docs/oracle.md)",
                fn_name, spec_ref.spec_fn_name,
            ),
            None => format!(
                "-- verify law {}.{}: trace-projection LHS is runtime-only (see docs/oracle.md)",
                fn_name, law_name,
            ),
        };
        return (header, case_index_start + vb.cases.len());
    }
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs);
    let theorem_base = match &spec_ref {
        Some(spec_ref) => format!(
            "{}_eq_{}",
            fn_name,
            aver_name_to_lean(&spec_ref.spec_fn_name)
        ),
        None => format!("{}_law_{}", fn_name, law_name),
    };
    // Oracle v1: rewrite calls to effectful fns in the law body so
    // they target the lifted form (see commit history in
    // `codegen/common.rs` / `codegen/dafny/toplevel.rs` for the
    // discovery that motivated this). Lemma body uses lemma-local
    // bindings; sample assertions use the concrete stub values.
    let law_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let law_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    // Refinement quantifier lift: when a given Int variable shows up
    // in the law body wrapped in a refinement-record constructor
    // (`Natural(value = a)`, `Positive(value = a)`, …), lift the
    // quantifier from the carrier type to the refined type so the
    // theorem statement reads `∀ (a : Natural), …` instead of
    // `∀ (a : Int), … Natural(value = a) …`. Strip the wrapper
    // constructor in the body templates so they read as
    // `add a b`, not `add (Natural(value = a)) (Natural(value = b))`.
    // The smart-constructor predicate ceases to be a per-case proof
    // obligation — it's already carried by the type.
    let mut lifted_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for given in &law.givens {
        if let Some(refined) = crate::codegen::common::refinement_lift_for_given(
            &given.name,
            &given.type_name,
            &law_lhs,
            &law_rhs,
            ctx,
        ) {
            lifted_vars.insert(given.name.clone(), refined.to_string());
        }
    }
    let law_lhs = if lifted_vars.is_empty() {
        law_lhs
    } else {
        crate::codegen::common::strip_refinement_wrappers(&law_lhs, &lifted_vars, ctx)
    };
    let law_rhs = if lifted_vars.is_empty() {
        law_rhs
    } else {
        crate::codegen::common::strip_refinement_wrappers(&law_rhs, &lifted_vars, ctx)
    };
    let lhs_template = emit_expr(&law_lhs, ctx);
    let rhs_template = emit_expr(&law_rhs, ctx);
    // The `when` clause references the same oracle bindings the law
    // body does, so it needs the same subtype projection. Without this
    // a `when rng(root, 0, 1, 6) >= 1` clause would emit `rng ...`
    // against a `RandomIntInBounds` parameter and Lean would reject
    // the type mismatch.
    let when_template = law.when.as_ref().map(|expr| {
        let oracle_projected = crate::codegen::common::rewrite_effectful_calls_in_law(
            expr,
            law,
            |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
            crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
        );
        // Refinement lift: bare references to lifted-given idents
        // inside `when`'s comparator BinOps need `.val` projection
        // because the quantifier is now over the Subtype carrier,
        // not the underlying Int. Without this `when a >= 10` over
        // `a : Natural` emits as `a >= 10` and fails to synthesize
        // `LE Natural` / `OfNat Natural 10` in Lean.
        let val_projected =
            crate::codegen::common::project_lifted_idents_to_val(&oracle_projected, &lifted_vars);
        emit_expr(&val_projected, ctx)
    });
    let quant_params = law
        .givens
        .iter()
        .map(|given| {
            // Oracle v1 + 0.13 subtype encoding: classified Generative-
            // shaped effect-givens bind oracles through a subtype
            // carrier (`RandomIntInBounds` etc.) that pairs the function
            // with the bound proof. The quantifier uses the subtype, so
            // the law's claim is universally quantified over only those
            // oracles that respect the bound — strictly stronger than
            // `∀ rng : BranchPath → Int → ... → Int`. Other effect
            // kinds (Output, unclassified) keep the plain function
            // signature.
            // Refinement lift: quant param `a` lifts from `Int` to
            // the wrapping refinement record (`Natural`, `Positive`,
            // …) when the law body used `Natural(value = a)` etc.
            // See `refinement_lift_for_given` for the detection.
            let type_text = if let Some(refined) = lifted_vars.get(&given.name) {
                refined.clone()
            } else if let Some(subtype) = bounded_oracle_subtype_for(&given.type_name) {
                subtype.to_string()
            } else {
                match crate::types::checker::effect_classification::oracle_signature(
                    &given.type_name,
                ) {
                    Some(oracle_ty) => crate::codegen::lean::types::type_to_lean(&oracle_ty),
                    None => type_annotation_to_lean(&given.type_name),
                }
            };
            format!("({} : {})", aver_name_to_lean(&given.name), type_text)
        })
        .collect::<Vec<_>>()
        .join(" ");

    match &spec_ref {
        Some(spec_ref) => lines.push(format!(
            "-- verify law {}.spec {} ({} cases)",
            fn_name,
            spec_ref.spec_fn_name,
            vb.cases.len()
        )),
        None => lines.push(format!(
            "-- verify law {}.{} ({} cases)",
            fn_name,
            law_name,
            vb.cases.len()
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
    if let Some(when_expr) = &law.when {
        lines.push(format!("-- when {}", emit_expr(when_expr, ctx)));
    }
    // Issue #128: singleton-domain givens + RHS that references no
    // given + IR didn't pin a strategy that closes the constant-RHS
    // shape ⇒ the universal is vacuous or false (e.g.
    // `checkRight L V R = Tree.Black Empty 1 Empty`) and the
    // structural-induction fallback can't close it. Skip; sample +
    // checked_domain cover the point. Strategies that DO close
    // constant-RHS shapes (Reflexive, Commutative, Associative,
    // MapUpdatePostcondition, …) stay in the keep-set; Induction
    // / BackendDispatch / Sorry don't.
    let ir_strategy_closes_const_rhs = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .is_some_and(|t| {
            !matches!(
                t.strategy,
                crate::ir::ProofStrategy::Induction { .. }
                    | crate::ir::ProofStrategy::BackendDispatch
                    | crate::ir::ProofStrategy::Sorry
            )
        });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    // Issue #128: a law that calls a fuel-bounded helper (a recursive
    // fn the proof-mode classifier rejected — `size`, `toSorted`,
    // `blackDepth`, …) can't be closed by the auto-proof matcher's
    // `induction t with …` chain: the goal stays under
    // `<fn>__fuel ((averMeasure _) * 3) …` which `simp` can't drive.
    // The expanded per-sample lemmas unfold fuel finitely (concrete
    // inputs) and stay decidable — skip the universal instead of
    // shipping `induction` tactics that don't close.
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    let calls_fuel_bounded = crate::codegen::common::law_calls_unclassified_fn(law, &unclassified);
    let skip_universal = singleton_const_rhs || calls_fuel_bounded;
    if !quant_params.is_empty() && !skip_universal {
        lines.extend(emit_verify_law_support_theorems(
            vb,
            law,
            ctx,
            &theorem_base,
        ));
        let theorem_prop = law_theorem_prop(
            law,
            ctx,
            &lhs_template,
            &rhs_template,
            when_template.as_deref(),
            &lifted_vars,
        );
        // Oracle v1: the auto-proof matchers compare law.lhs / law.rhs
        // ASTs. For effectful laws the theorem statement has been
        // rewritten to target the lifted fn (BranchPath.root() + oracle
        // args injected); the matchers need to see the same rewritten
        // form or they'll miss shapes like
        // `pickOne(root, rnd) == pickOneSpec(root, rnd)` and fall back
        // to `sorry`. Build a view of the law with the rewritten body.
        let law_for_auto_proof = crate::ast::VerifyLaw {
            name: law.name.clone(),
            givens: law.givens.clone(),
            when: law.when.clone(),
            lhs: law_lhs.clone(),
            rhs: law_rhs.clone(),
            sample_guards: law.sample_guards.clone(),
        };
        // (Removed: refinement_auto_proof — Aver-specific bypass.
        // Refinement-lifted laws now flow through law_auto via the
        // IR-pinned `ProofStrategy::LinearArithmetic { lifted: true }`.
        // The lowerer detects the `Refined(value = given)` carrier
        // shape and pins the strategy; emit_simp_omega_from_ir
        // skips by_cases when lifted=true. Step 30 retired this
        // separate code path so all laws go through one dispatch.)
        if let Some(auto_proof) = emit_verify_law_forall_auto_proof(
            vb,
            &law_for_auto_proof,
            ctx,
            verify_mode,
            &theorem_base,
            &quant_params,
            &theorem_prop,
        ) {
            lines.extend(auto_proof.support_lines);
            if !auto_proof.replaces_theorem {
                lines.push(format!(
                    "theorem {} : ∀ {}, {} := by",
                    theorem_base, quant_params, theorem_prop
                ));
            }
            lines.extend(auto_proof.proof_lines);
        } else {
            lines.push(format!(
                "theorem {} : ∀ {}, {} := by",
                theorem_base, quant_params, theorem_prop
            ));
            lines.push(
                "  -- verify law is sampled; universal proof must be provided manually".to_string(),
            );
            lines.push("  sorry".to_string());
        }
    }

    // Skip checked_domain emission for refinement-lifted laws: the
    // universal theorem already quantifies over the refined type
    // (`∀ a b : Natural`), which strictly entails any
    // sample-domain conjunction over the same body. Keeping
    // checked_domain would emit a 25+ conjunct of `add ⟨v, by …⟩`
    // calls that Lean has to run `native_decide` against, which
    // for compound predicates (`Bool.and(n ≥ 0, n ≤ 100)`) blows
    // through `maxHeartbeats`. The per-case `sample_N` theorems
    // below still get emitted as a granular cross-check.
    if !vb.cases.is_empty() && lifted_vars.is_empty() {
        let domain_theorem_name = format!("{}_checked_domain", theorem_base);
        let domain_prop = vb
            .cases
            .iter()
            .enumerate()
            .map(|(idx, (left, right))| {
                // Oracle v1: per-case sample rewrite. Each case has
                // its own domain-value binding (one case per value
                // in `given stub: E = [a, b, ...]`). Use the case's
                // own binding map rather than the law-level `.first()`
                // which would emit cross-case mismatches.
                let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
                let mode =
                    crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
                let left_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
                    left,
                    law,
                    |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
                    mode.clone(),
                );
                let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
                    right,
                    law,
                    |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
                    mode,
                );
                let left_str = emit_expr(&left_rw, ctx);
                let right_str = emit_expr(&right_rw, ctx);
                if let Some(guard) = law.sample_guards.get(idx) {
                    format!(
                        "({} = true -> {} = {})",
                        emit_expr(guard, ctx),
                        left_str,
                        right_str
                    )
                } else {
                    format!("({} = {})", left_str, right_str)
                }
            })
            .collect::<Vec<_>>()
            .join(" ∧ ");
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                // `checked_domain` is one nested ∧-conjunction with N
                // implications. When the law's case body produces a
                // wrapper type (Result → `Except String T`, Option →
                // `Option T`), Lean's default `synthInstance.maxSize`
                // (~200) is too small to reach `DecidableEq` through
                // the wrapper instance at ~16+ conjuncts — `native_
                // decide` then dies with `failed to synthesize
                // Decidable`. Bumping the budget locally to the theorem
                // is cheaper than rewriting the emitter to fan the
                // cases out into N separate theorems (the per-case
                // `sample_N` theorems below already cover that view).
                lines.push(format!(
                    "set_option synthInstance.maxSize 4096 in\ntheorem {} : {} := by native_decide",
                    domain_theorem_name, domain_prop
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "theorem {} : {} := by sorry",
                    domain_theorem_name, domain_prop
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                lines.push(format!(
                    "theorem {} : {} := by",
                    domain_theorem_name, domain_prop
                ));
                lines.push("  sorry".to_string());
            }
        }
    }

    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        let theorem_name = format!("{}_sample_{}", theorem_base, case_index_start + idx + 1);
        // Oracle v1: inject the case-specific stub value so a domain
        // like `given stub: E = [a, b]` produces two sample theorems —
        // one with `a`, one with `b` — instead of mismatched
        // `impl(…, a) = spec(…, b)` pairs.
        let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
        let mode = crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
        let left_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            left,
            law,
            |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
            mode.clone(),
        );
        let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            law,
            |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
            mode,
        );
        let left_str = emit_expr(&left_rw, ctx);
        let right_str = emit_expr(&right_rw, ctx);
        let sample_prop = if let Some(guard) = law.sample_guards.get(idx) {
            format!(
                "{} = true -> {} = {}",
                emit_expr(guard, ctx),
                left_str,
                right_str
            )
        } else {
            format!("{} = {}", left_str, right_str)
        };
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "theorem {} : {} := by native_decide",
                    theorem_name, sample_prop
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "theorem {} : {} := by sorry",
                    theorem_name, sample_prop
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                lines.push(format!("theorem {} : {} := by", theorem_name, sample_prop));
                lines.push("  sorry".to_string());
            }
        }
    }
    (lines.join("\n"), case_index_start + vb.cases.len())
}

fn law_theorem_prop(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    lhs_template: &str,
    rhs_template: &str,
    when_template: Option<&str>,
    lifted_vars: &std::collections::HashMap<String, String>,
) -> String {
    let mut premises = Vec::new();
    let when_redundant_with_lifts = law
        .when
        .as_ref()
        .map(|w| {
            crate::codegen::common::when_is_redundant_with_refinement_lifts(w, lifted_vars, ctx)
        })
        .unwrap_or(false);
    if law.when.is_some() {
        // Lifted vars are quantified over the refinement record
        // (`a : Natural`), not the carrier `Int`, so the disjunctive
        // domain premise (`a = 0 ∨ a = 1 ∨ …`) is type-mismatched
        // (Lean sees `0 : Int` against `a : Natural`). Skip the
        // domain premise for any lifted given.
        premises.extend(law.givens.iter().filter_map(|given| {
            if lifted_vars.contains_key(&given.name) {
                None
            } else {
                Some(law_given_domain_prop(given, ctx))
            }
        }));
    }
    // `when` drop is only sound when the predicate is syntactically
    // equivalent (via commutator-relaxed compare) to the conjunction
    // of lifted givens' refinement invariants — otherwise stronger /
    // orthogonal user predicates would be silently lost from the
    // emitted theorem (e.g. `when a >= 10` over `a : Natural` whose
    // invariant is `a.val >= 0`). Same identity check the Dafny
    // backend uses.
    if let Some(when_expr) = when_template
        && !when_redundant_with_lifts
    {
        premises.push(format!("{when_expr} = true"));
    }
    let conclusion = format!("{lhs_template} = {rhs_template}");
    if premises.is_empty() {
        conclusion
    } else {
        format!("{} -> {}", premises.join(" -> "), conclusion)
    }
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

fn law_given_domain_prop(given: &VerifyGiven, ctx: &CodegenContext) -> String {
    let raw_name = aver_name_to_lean(&given.name);
    // Subtype-carried oracle bindings (`rng : RandomIntInBounds`) need
    // `.val` projection on the LHS of the equality so the comparison
    // type-checks against the underlying plain function the user's
    // stub delivers. Other givens compare the raw value directly.
    let given_name = if bounded_oracle_subtype_for(&given.type_name).is_some() {
        format!("{raw_name}.val")
    } else {
        raw_name
    };
    let values = law_given_domain_values(&given.domain);
    match values.as_slice() {
        [] => "False".to_string(),
        [value] => format!("{given_name} = {}", emit_expr(value, ctx)),
        _ => values
            .iter()
            .map(|value| format!("{given_name} = {}", emit_expr(value, ctx)))
            .collect::<Vec<_>>()
            .join(" ∨ "),
    }
}

pub(super) fn law_given_domain_values(domain: &VerifyGivenDomain) -> Vec<Spanned<Expr>> {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
            .map(|n| Spanned::bare(Expr::Literal(Literal::Int(n))))
            .collect(),
        VerifyGivenDomain::Explicit(values) => values.clone(),
    }
}

/// Emit a decision block as a Lean 4 block comment.
pub fn emit_decision(db: &DecisionBlock) -> String {
    let mut lines = Vec::new();
    lines.push(format!("/- Decision: {}", db.name));
    lines.push(format!("   Date: {}", db.date));
    lines.push(format!("   Reason: {}", db.reason));
    lines.push(format!("   Chosen: {}", db.chosen.node.as_context_string()));
    if !db.rejected.is_empty() {
        lines.push(format!(
            "   Rejected: {}",
            db.rejected
                .iter()
                .map(|r| r.node.as_context_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if !db.impacts.is_empty() {
        let impacts = db
            .impacts
            .iter()
            .map(|impact| impact.node.as_context_string())
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
        let body = emit_fn_body_for(fd, &fd.body, ctx);
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
pub fn emit_mutual_group_proof(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    // Distinguish mutual SCC shapes by the Lex params vector:
    //   `[p]` rank 0  → MutualIntCountdown
    //   `[s, pos]`    → MutualStringPosAdvance
    //   `[]` rank >=1 → MutualSizeOfRanked
    let all_int_countdown = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, 0)) if params.len() == 1
        )
    });
    if all_int_countdown {
        return emit_fuelized_mutual_int_countdown_group(fns, ctx);
    }

    let all_string_pos = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.len() == 2
        )
    });
    if all_string_pos {
        return emit_fuelized_mutual_string_pos_group(fns, ctx);
    }

    let all_sizeof = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.is_empty()
        )
    });
    if all_sizeof {
        if let Some(code) = emit_native_mutual_sizeof_group(fns, ctx) {
            return code;
        }
        return emit_fuelized_mutual_sizeof_group(fns, ctx);
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
        let body = emit_fn_body_for(fd, &fd.body, ctx);
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        match contract_lex_params_rank(ctx, fd) {
            Some((params, 0)) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param. (The IR's param name is
                // canonical; we don't fall back to fd.params here.)
                let lean_first = aver_name_to_lean(&params[0]);
                lines.push(format!("  termination_by Int.natAbs {}", lean_first));
                lines.push("  decreasing_by".to_string());
                lines.push("    omega".to_string());
            }
            Some((params, rank)) if params.len() == 2 => {
                // MutualStringPosAdvance — (s, pos) shape; rank
                // distinguishes SCC members.
                let lean_s = aver_name_to_lean(&params[0]);
                let lean_pos = aver_name_to_lean(&params[1]);
                lines.push(format!(
                    "  termination_by (({}.data.length) - ({}.toNat), {})",
                    lean_s, lean_pos, rank
                ));
                lines.push("  decreasing_by".to_string());
                lines.push("    simp_wf".to_string());
            }
            Some(([], _)) => {
                // MutualSizeOfRanked — handled inside the SCC's
                // dedicated emitter; no termination_by suffix here.
            }
            _ => {}
        }
        lines.push(String::new());
    }

    lines.push("end".to_string());
    lines.join("\n")
}

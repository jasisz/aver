//! Dafny-side counterpart of Lean's mutual fuel-guarded emission.
//!
//! For every mutual-recursion SCC the shared classifier tags with a
//! `Mutual*` plan, we emit:
//!
//! ```dafny
//! function f__fuel(fuel: nat, args): T
//!   decreases fuel
//! {
//!   if fuel == 0 then <total default for T>
//!   else var fuel' := fuel - 1; <body with rec calls → g__fuel(fuel', …)>
//! }
//!
//! function f(args): T {
//!   f__fuel(<plan metric>, args)
//! }
//! ```
//!
//! Rank-based plans (MutualStringPosAdvance, MutualSizeOfRanked) scale
//! the metric by the SCC size so a full traversal terminates before
//! fuel hits zero. The `fuel == 0` default-value branch ensures the
//! function is total; callers that pass enough fuel never hit it.
//!
//! Fns whose return type has no obvious total default (opaque Named
//! datatypes with non-trivial invariants) still fall back to
//! `function {:axiom}` — generating a meaningful default would require
//! walking the whole ADT graph, which we defer to a follow-up.
//!
//! Parallels `codegen::lean::toplevel::emit_fuelized_mutual_*`.

use std::collections::HashSet;

use super::expr::aver_name_to_dafny;
use crate::ast::{FnDef, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::common::parse_type_annotation;
use crate::codegen::recursion::{RecursionPlan, fuel_helper_name, rewrite_recursive_calls_body};
use crate::types::Type;

/// Emit the whole SCC as a fuel-guarded mutual group. The helper
/// definitions go first (Dafny resolves identifiers by file order but
/// accepts forward references inside the same file; grouping helpers
/// keeps the output readable). Wrappers follow immediately so readers
/// see the supplied fuel metric right next to the fn's public shape.
///
/// Returns `None` if any fn in the SCC has a return type without a
/// total default in Dafny — those still have to fall back to axiom.
pub fn emit_mutual_fuel_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
    plans: &std::collections::HashMap<String, RecursionPlan>,
) -> Option<String> {
    // Totality guard: every fn's return type needs a value to pick on
    // fuel exhaustion. Named ADTs walk their first variant; left-
    // recursive ADTs without a base variant first, or fn types, still
    // refuse — the caller axiomizes those groups.
    for fd in fns {
        dafny_default_value(&fd.return_type, ctx)?;
    }

    let scc_size = fns.len();
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();

    let mut helper_lines: Vec<String> = Vec::new();
    let mut wrapper_lines: Vec<String> = Vec::new();

    for fd in fns {
        let plan = plans
            .get(&fd.name)
            .cloned()
            .unwrap_or(RecursionPlan::MutualSizeOfRanked { rank: 1 });
        let helper_name = fuel_helper_name(&fd.name);
        let fn_name = aver_name_to_dafny(&fd.name);
        let params_str = emit_dafny_params(&fd.params);
        let ret_type_str = super::toplevel::emit_type(&fd.return_type);
        let default_val = dafny_default_value(&fd.return_type, ctx)
            .expect("default value presence is checked above");
        let arg_names = emit_dafny_arg_names(&fd.params);
        let metric = emit_fuel_metric(fd, &plan, scc_size);

        // Apply `?!` lowering first (same step the non-fuel pure-fn
        // emission runs), then rewrite intra-SCC calls to reference
        // the fuel helper. Without the lowering, `pair = (f(x),
        // g(rest))?!` stays an `IndependentProduct` node in the AST
        // and Dafny sees raw tuples instead of the match-unwrapped
        // Result shape.
        let lowered_body = crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
            .ok()
            .flatten()
            .map(|lowered| lowered.body.as_ref().clone())
            .unwrap_or_else(|| fd.body.as_ref().clone());
        let rewritten_body = rewrite_recursive_calls_body(&lowered_body, &targets, "fuel'");
        let body_str = super::toplevel::emit_fn_body(&rewritten_body, ctx);

        if let Some(desc) = &fd.desc {
            helper_lines.push(format!("// {}", desc));
        }
        helper_lines.push(format!(
            "function {}(fuel: nat, {}): {}",
            helper_name, params_str, ret_type_str
        ));
        helper_lines.push("  decreases fuel".to_string());
        helper_lines.push("{".to_string());
        helper_lines.push(format!("  if fuel == 0 then {}", default_val));
        helper_lines.push(format!("  else var fuel' := fuel - 1; {}", body_str));
        helper_lines.push("}\n".to_string());

        wrapper_lines.push(format!(
            "function {}({}): {}",
            fn_name, params_str, ret_type_str
        ));
        wrapper_lines.push("{".to_string());
        wrapper_lines.push(format!("  {}({}, {})", helper_name, metric, arg_names));
        wrapper_lines.push("}\n".to_string());
    }

    Some(
        [helper_lines, wrapper_lines]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

fn emit_dafny_params(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(pname, ptype)| {
            format!(
                "{}: {}",
                aver_name_to_dafny(pname),
                super::toplevel::emit_type(ptype)
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_dafny_arg_names(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(pname, _)| aver_name_to_dafny(pname))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Fuel metric to supply from the wrapper. The concrete formula
/// depends on the plan variant:
///
/// - `MutualIntCountdown`: `natAbs(n) + 1` on the first Int param.
/// - `MutualStringPosAdvance { rank }`: `(|s| + 1) * rank`.
/// - `MutualSizeOfRanked { rank }`: `(|first_seq| + 1) * rank * scc_size`.
///
/// We multiply by `scc_size` on `MutualSizeOfRanked` because a single
/// walk through the SCC may dispatch each fn once per element; a
/// conservative upper bound keeps us on the safe side of Dafny's
/// `decreases fuel` check even when the classifier's rank analysis is
/// loose.
fn emit_fuel_metric(fd: &FnDef, plan: &RecursionPlan, scc_size: usize) -> String {
    match plan {
        RecursionPlan::MutualIntCountdown => {
            let Some(param) = first_int_param(fd) else {
                return "1".to_string();
            };
            let name = aver_name_to_dafny(param);
            format!("(if {n} >= 0 then {n} else 0) + 1", n = name)
        }
        RecursionPlan::MutualStringPosAdvance { rank }
        | RecursionPlan::MutualSizeOfRanked { rank } => {
            let Some(name) = first_seq_or_string_param(fd) else {
                return format!("{}", rank.max(&1));
            };
            format!(
                "(|{n}| + 1) * {budget}",
                n = aver_name_to_dafny(name),
                budget = rank * scc_size + 1
            )
        }
        _ => "1".to_string(),
    }
}

fn first_int_param(fd: &FnDef) -> Option<&String> {
    fd.params
        .iter()
        .find(|(_, t)| parse_type_annotation(t) == Type::Int)
        .map(|(n, _)| n)
}

fn first_seq_or_string_param(fd: &FnDef) -> Option<&String> {
    fd.params
        .iter()
        .find(|(_, t)| {
            let ty = parse_type_annotation(t);
            matches!(ty, Type::List(_) | Type::Vector(_) | Type::Str)
        })
        .map(|(n, _)| n)
}

/// A Dafny expression that is a valid inhabitant of `type_str` — used
/// as the `fuel == 0` branch in the fuel helper so the function stays
/// total. Returns `None` when the type's inhabitants depend on a
/// recursive ADT the first-variant walk can't break out of, a fn type,
/// or an unknown type. For user-defined datatypes the walk picks the
/// first variant (for sum types) or fills every field with its own
/// default (for products), recursing through Named types and guarding
/// against infinite recursion with a visiting-set.
pub fn dafny_default_value(type_str: &str, ctx: &CodegenContext) -> Option<String> {
    let mut visiting = HashSet::new();
    type_default(&parse_type_annotation(type_str), ctx, &mut visiting)
}

fn type_default(ty: &Type, ctx: &CodegenContext, visiting: &mut HashSet<String>) -> Option<String> {
    Some(match ty {
        Type::Int => "0".to_string(),
        Type::Float => "0.0".to_string(),
        Type::Str => "\"\"".to_string(),
        Type::Bool => "false".to_string(),
        Type::Unit => "()".to_string(),
        Type::List(_) | Type::Vector(_) => "[]".to_string(),
        Type::Map(_, _) => "map[]".to_string(),
        Type::Option(_) => "Option.None".to_string(),
        Type::Result(_, err) => {
            format!("Result.Err({})", type_default(err, ctx, visiting)?)
        }
        Type::Tuple(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|t| type_default(t, ctx, visiting))
                .collect::<Option<_>>()?;
            format!("({})", parts.join(", "))
        }
        Type::Named(name) => named_type_default(name, ctx, visiting)?,
        Type::Fn(_, _, _) | Type::Unknown | Type::Var(_) => return None,
    })
}

fn named_type_default(
    name: &str,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> Option<String> {
    if visiting.contains(name) {
        // Left-recursive ADT (e.g. first variant references the type
        // itself before a base case). Refuse the default — the SCC
        // will fall back to axiom emission, the parallel of Lean's
        // `partial def`.
        return None;
    }
    visiting.insert(name.to_string());
    let td = find_type_def(ctx, name)?;
    let result = type_def_default(td, ctx, visiting);
    visiting.remove(name);
    result
}

fn find_type_def<'a>(ctx: &'a CodegenContext, target: &str) -> Option<&'a TypeDef> {
    ctx.type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .find(|td| crate::codegen::common::type_def_name(td) == target)
}

fn type_def_default(
    td: &TypeDef,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> Option<String> {
    match td {
        TypeDef::Sum { name, variants, .. } => {
            // Pick the first variant — conventionally the base case in
            // Aver sources. For variants that reference the enclosing
            // type before any base case, the visiting-set catches it
            // and the whole SCC falls back to axiom.
            let variant = variants.first()?;
            if variant.fields.is_empty() {
                Some(format!("{}.{}", name, variant.name))
            } else {
                let args: Vec<String> = variant
                    .fields
                    .iter()
                    .map(|ft| type_default(&parse_type_annotation(ft), ctx, visiting))
                    .collect::<Option<_>>()?;
                Some(format!("{}.{}({})", name, variant.name, args.join(", ")))
            }
        }
        TypeDef::Product { name, fields, .. } => {
            // Dafny datatype product syntax: `Name(field := value, …)`.
            let args: Vec<String> = fields
                .iter()
                .map(|(fname, ftype)| {
                    type_default(&parse_type_annotation(ftype), ctx, visiting)
                        .map(|v| format!("{} := {}", aver_name_to_dafny(fname), v))
                })
                .collect::<Option<_>>()?;
            Some(format!("{}({})", name, args.join(", ")))
        }
    }
}

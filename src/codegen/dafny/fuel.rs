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
use super::toplevel::emit_fn_def_axiom;
use crate::ast::FnDef;
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
    // fuel exhaustion. Named ADTs are the tricky case — we'd need to
    // construct a default variant recursively, which is a separate
    // pass. For now, refuse the fuel emission and let the caller
    // axiomize the group.
    for fd in fns {
        dafny_default_value(&fd.return_type)?;
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
        let default_val =
            dafny_default_value(&fd.return_type).expect("default value presence is checked above");
        let arg_names = emit_dafny_arg_names(&fd.params);
        let metric = emit_fuel_metric(fd, &plan, scc_size);

        let rewritten_body = rewrite_recursive_calls_body(&fd.body, &targets, "fuel'");
        let body_str = super::toplevel::emit_fn_body_pub(&rewritten_body, ctx);

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

    // Fallback comment for each fn that failed above — handled upstream.
    for fd in fns {
        if dafny_default_value(&fd.return_type).is_none() {
            helper_lines.push(emit_fn_def_axiom(fd));
        }
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
/// total. Returns `None` when the type's inhabitants depend on user-
/// defined constructors (Named ADTs) or function types, where picking
/// a default isn't obviously defensible without further analysis.
pub fn dafny_default_value(type_str: &str) -> Option<String> {
    type_default(&parse_type_annotation(type_str))
}

fn type_default(ty: &Type) -> Option<String> {
    Some(match ty {
        Type::Int => "0".to_string(),
        Type::Float => "0.0".to_string(),
        Type::Str => "\"\"".to_string(),
        Type::Bool => "false".to_string(),
        Type::Unit => "()".to_string(),
        Type::List(_) | Type::Vector(_) => "[]".to_string(),
        Type::Map(_, _) => "map[]".to_string(),
        Type::Option(_) => "Option.None".to_string(),
        Type::Result(_, err) => format!("Result.Err({})", type_default(err)?),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(type_default).collect::<Option<_>>()?;
            format!("({})", parts.join(", "))
        }
        Type::Named(_) | Type::Fn(_, _, _) | Type::Unknown => return None,
    })
}

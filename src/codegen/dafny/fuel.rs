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
use crate::codegen::recursion::{fuel_helper_name, rewrite_recursive_calls_body};
use crate::types::Type;

/// Emit the whole SCC as a fuel-guarded mutual group. The helper
/// definitions go first (Dafny resolves identifiers by file order but
/// accepts forward references inside the same file; grouping helpers
/// keeps the output readable). Wrappers follow immediately so readers
/// see the supplied fuel metric right next to the fn's public shape.
///
/// Returns `None` if any fn in the SCC has a return type without a
/// total default in Dafny — those still have to fall back to axiom.
pub fn emit_mutual_fuel_group(fns: &[&FnDef], ctx: &CodegenContext) -> Option<String> {
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
        let helper_name = fuel_helper_name(&fd.name);
        let fn_name = aver_name_to_dafny(&fd.name);
        let params_str = emit_dafny_params(&fd.params);
        let ret_type_str = super::toplevel::emit_type(&fd.return_type);
        let default_val = dafny_default_value(&fd.return_type, ctx)
            .expect("default value presence is checked above");
        let arg_names = emit_dafny_arg_names(&fd.params);
        let metric = emit_fuel_metric(fd, ctx, scc_size);

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
        let active = ctx.active_module_scope();
        let resolved_body = ctx.resolve_rewritten_fn_body(&rewritten_body, active.as_deref());
        let body_str = super::toplevel::emit_fn_body(&resolved_body, ctx);

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

/// Native `decreases` tuple emission for mutual-recursion SCCs whose
/// every fn has a sizeOf measure on at least one parameter and the
/// classifier already gave each fn a rank in the SCC. Replaces the
/// fuel-bounded encoding entirely for these groups: Dafny verifies
/// termination via the lexicographic tuple `(measure, rank)` and Z3
/// can symbolically unfold the SCC for proofs over concrete values
/// without hitting a fuel ceiling.
///
/// Returns `None` when the SCC isn't `MutualSizeOfRanked`, or when
/// any member has no inferable sizeOf measure (no `List`/`Vector`/
/// `String` parameter). The caller falls back to
/// [`emit_mutual_fuel_group`] in that case.
///
/// Why ranks come from the classifier verbatim: `ranks_from_same_
/// edges` already topo-sorts on "same-measure" callees, so a call
/// that keeps the measure constant (e.g. `addStep → addDigits` on
/// the same `(ta, tb)`) goes to a strictly lower rank — the lex
/// tuple decreases on the second component. Calls that strictly
/// shrink the measure (e.g. `addDigits → addLeft` peeling a head
/// off `a`) decrease on the first component regardless of rank.
pub fn emit_mutual_native_decreases_group(fns: &[&FnDef], ctx: &CodegenContext) -> Option<String> {
    // MutualSizeOfRanked SCC: every member's contract is
    // `Fuel { Lex { params: [], rank } }` (empty params signal the
    // frame-level sizeOf measure). Any other Lex shape (single-param
    // mutual int-countdown, two-param string-pos) fails this group.
    let mut ranks: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for fd in fns {
        let contract = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?;
        match contract.recursion.as_ref()? {
            crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::Lex { params, rank },
            } if params.is_empty() => {
                ranks.insert(fd.name.clone(), *rank);
            }
            _ => return None,
        }
    }
    let mut measures: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for fd in fns {
        let measure = emit_sizeof_measure_expr_dafny(fd)?;
        measures.insert(fd.name.clone(), measure);
    }

    // Reject tail-recursive accumulator patterns where an intra-SCC
    // call passes a sizeOf-relevant arg through `[x] + acc` /
    // `List.prepend(x, acc)` / `acc + [x]` — measure grows across
    // that edge so the lexicographic `(measure, rank)` tuple can't
    // decrease and Dafny rejects the `decreases` clause. The fuel-
    // bounded encoding still terminates these (fuel parameter
    // counts down regardless of measure shape), so we fall back to
    // it for these SCCs. Conservative: any non-`Ident`/`Resolved`/
    // `Literal`/`Attr` arg in a sizeOf slot drops us back to fuel.
    if crate::codegen::recursion::detect::scc_has_growing_accumulator(fns) {
        return None;
    }

    // The contract's rank orders the calls that leave the PLAN's measure
    // unchanged. Dafny's measure is the length of every sequence parameter,
    // which is what the plan ranked whenever it ranked the parameters the
    // members hand on by name — the ordering these groups had before the
    // call edge analysis existed, kept as it was. A rank the analysis chose
    // for another measure — one counting an `Int` down, or leaving a
    // sequence out — is not Dafny's to pair with `|xs| + |ys|`: there the
    // analysis is rerun over exactly Dafny's parameters, and the group stays
    // native only when it counts every one of them, with the ranks it then
    // gives.
    if crate::codegen::recursion::detect::forwarded_ranks(fns).is_none() {
        ranks = dafny_cycle_ranks(fns)?;
    }

    let mut lines: Vec<String> = Vec::new();
    for fd in fns {
        let measure = measures.get(&fd.name).unwrap();
        let rank = ranks.get(&fd.name).unwrap();
        let fn_name = aver_name_to_dafny(&fd.name);
        let params_str = emit_dafny_params(&fd.params);
        let ret_type_str = super::toplevel::emit_type(&fd.return_type);
        // Apply `?!` lowering so the body shape matches what the
        // fuel path also emits — keeps `(f(x), g(y))?!` -> match-
        // unwrapped tuple even on the native path.
        let lowered_body = crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
            .ok()
            .flatten()
            .map(|lowered| lowered.body.as_ref().clone())
            .unwrap_or_else(|| fd.body.as_ref().clone());
        let active = ctx.active_module_scope();
        let resolved_body = ctx.resolve_rewritten_fn_body(&lowered_body, active.as_deref());
        let body_str = super::toplevel::emit_fn_body(&resolved_body, ctx);

        if let Some(desc) = &fd.desc {
            lines.push(format!("// {}", desc));
        }
        lines.push(format!(
            "function {}({}): {}",
            fn_name, params_str, ret_type_str
        ));
        lines.push(format!("  decreases {}, {}", measure, rank));
        lines.push("{".to_string());
        lines.push(format!("  {}", body_str));
        lines.push("}\n".to_string());
    }
    Some(lines.join("\n"))
}

/// SizeOf measure expression for a fn's `List`/`Vector`/`String`
/// parameters — Dafny syntax `|name|` for seq/string length. Matches
/// the index set [`sizeof_measure_param_indices`] uses on the Lean
/// side so the same fns are picked across backends.
fn emit_sizeof_measure_expr_dafny(fd: &FnDef) -> Option<String> {
    let terms: Vec<String> = dafny_measure_param_indices(fd)
        .into_iter()
        .map(|index| format!("|{}|", aver_name_to_dafny(&fd.params[index].0)))
        .collect();
    (!terms.is_empty()).then(|| terms.join(" + "))
}

/// The positions Dafny's `decreases` measures: every `List`, `Vector` and
/// `String` parameter, in signature order.
fn dafny_measure_param_indices(fd: &FnDef) -> Vec<usize> {
    fd.params
        .iter()
        .enumerate()
        .filter(|(_, (_, ty))| {
            matches!(
                crate::codegen::common::parse_type_annotation(ty),
                crate::types::Type::List(_)
                    | crate::types::Type::Vector(_)
                    | crate::types::Type::Str
            )
        })
        .map(|(index, _)| index)
        .collect()
}

/// Tie-break ranks from the call edge analysis run over exactly the
/// parameters Dafny measures, or `None` when the measure it finds counts
/// something else (an `Int` it can see count down) or leaves one of them
/// out — `decreases |xs| + |ys|, rank` is only right for a rank that
/// orders the calls leaving `|xs| + |ys|` unchanged. The analysis is run
/// with a matched list's head not taken for a part of the list: the head
/// is smaller by size, which the plan measures, not by length.
fn dafny_cycle_ranks(fns: &[&FnDef]) -> Option<std::collections::HashMap<String, usize>> {
    use crate::codegen::recursion::cycle_measure::{Candidate, MeasureKind, measure_for_cycle};
    let candidates: Vec<Vec<Candidate>> = fns
        .iter()
        .map(|fd| {
            fd.params
                .iter()
                .enumerate()
                .filter_map(|(index, (_, ty))| {
                    let kind = match crate::codegen::common::parse_type_annotation(ty) {
                        crate::types::Type::List(_) | crate::types::Type::Vector(_) => {
                            MeasureKind::Structural
                        }
                        crate::types::Type::Int => MeasureKind::Countdown,
                        _ => return None,
                    };
                    Some(Candidate { index, kind })
                })
                .collect()
        })
        .collect();
    let measures = measure_for_cycle(fns, &candidates, false).ok()?;
    let counted: Vec<Vec<usize>> = measures
        .iter()
        .map(|measure| measure.params.iter().map(|c| c.index).collect())
        .collect();
    let measured: Vec<Vec<usize>> = fns
        .iter()
        .map(|fd| dafny_measure_param_indices(fd))
        .collect();
    (counted == measured).then(|| {
        fns.iter()
            .zip(measures)
            .map(|(fd, measure)| (fd.name.clone(), measure.rank))
            .collect()
    })
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
fn emit_fuel_metric(fd: &FnDef, ctx: &CodegenContext, scc_size: usize) -> String {
    // Reads the contract's Lex shape and emits the appropriate Dafny
    // fuel formula. Distinguishes by `params` length (the producer
    // partition from Step 12):
    //
    // - `[p]` rank 0  → MutualIntCountdown: `natAbs(n) + 1`
    // - `[s, pos]`    → MutualStringPosAdvance: `(|s| + 1) * (rank * scc_size + 1)`
    // - `[]`          → MutualSizeOfRanked: `(|first_seq| + 1) * (rank * scc_size + 1)`
    let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd) else {
        return "1".to_string();
    };
    let Some(crate::ir::RecursionContract::Fuel {
        fuel_metric: crate::ir::FuelMetric::Lex { params, rank },
    }) = contract.recursion.as_ref()
    else {
        return "1".to_string();
    };
    match params.as_slice() {
        [p] if *rank == 0 => {
            // MutualIntCountdown — first Int param countdown.
            let name = aver_name_to_dafny(p);
            format!("(if {n} >= 0 then {n} else 0) + 1", n = name)
        }
        [_s, _pos] => {
            // MutualStringPosAdvance — fuel formula `(|s| + 1) *
            // budget`. Falls back to a sequence param if the IR's
            // stored `s` name doesn't resolve.
            let Some(name) = first_seq_or_string_param(fd) else {
                return format!("{}", rank.max(&1));
            };
            format!(
                "(|{n}| + 1) * {budget}",
                n = aver_name_to_dafny(name),
                budget = rank * scc_size + 1
            )
        }
        [] => {
            // MutualSizeOfRanked — frame-level sizeOf measure.
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
        Type::Named { .. } => {
            let key = crate::codegen::common::backend_named_type_key(ctx, ty)?;
            named_type_default(&key, ctx, visiting)?
        }
        Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => return None,
    })
}

fn named_type_default(
    name: &str,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> Option<String> {
    // Resolve first, then guard: the key has to be the declaration we
    // actually reached, not the bare name that led here. Two modules may
    // declare the same name, and keying the guard on the bare form made one
    // of them look like a cycle the other was already inside — a refusal,
    // and so an axiom, for a type that has a perfectly good default.
    let td = find_type_def(ctx, name)?;
    let key = crate::codegen::common::type_key_for_decl(ctx, td).canonical();
    if visiting.contains(&key) {
        // Left-recursive ADT (e.g. first variant references the type
        // itself before a base case). Refuse the default — the SCC
        // will fall back to axiom emission, the parallel of Lean's
        // `partial def`.
        return None;
    }
    visiting.insert(key.clone());
    let result = type_def_default(td, ctx, visiting);
    visiting.remove(&key);
    result
}

/// Locate a `TypeDef` by canonical or bare name.
///
/// Epic #180 Phase 6 — accepts dotted canonical keys
/// (`"A.Shape"` for module-owned types) routed to the matching
/// module's `type_defs` list, so cross-module same-bare-name
/// types resolve to the correct declaration instead of the
/// first-match-wins bare lookup. Bare keys (entry-scope types,
/// builtins) still resolve via the entry → module-walk fallback
/// chain.
fn find_type_def<'a>(ctx: &'a CodegenContext, target: &str) -> Option<&'a TypeDef> {
    if let Some((prefix, bare)) = target.rsplit_once('.') {
        for m in &ctx.modules {
            if m.prefix == prefix {
                return m
                    .type_defs
                    .iter()
                    .find(|td| crate::codegen::common::type_def_name(td) == bare);
            }
        }
    }
    // A bare name means whatever the module being emitted can see. The
    // emitter already tracks that scope (`with_module_scope` wraps this
    // whole walk); reading it here is what keeps a declaration the asker
    // never imports from answering. Its own types come first, then its
    // `depends` closure, and the entry last — the entry is another module
    // from a dependency's point of view. jasisz/aver#1134.
    let scope = ctx.active_module_scope();
    if let Some(scope) = scope.as_deref()
        && let Some(module) = ctx.modules.iter().find(|m| m.prefix == scope)
        && let Some(td) = module
            .type_defs
            .iter()
            .find(|td| crate::codegen::common::type_def_name(td) == target)
    {
        return Some(td);
    }
    match crate::codegen::common::visible_module_prefixes(scope.as_deref(), ctx) {
        // Emitting a dependency: only what it depends on. The entry is
        // another module from its point of view and never names it.
        Some(prefixes) => ctx
            .modules
            .iter()
            .filter(|m| prefixes.contains(&m.prefix))
            .flat_map(|m| m.type_defs.iter())
            .find(|td| crate::codegen::common::type_def_name(td) == target),
        // Emitting the entry: everything loaded is there because the entry
        // reaches it, and the entry's own declarations come first.
        None => ctx
            .type_defs
            .iter()
            .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
            .find(|td| crate::codegen::common::type_def_name(td) == target),
    }
}

#[cfg(test)]
mod resolution_tests {
    use super::find_type_def;
    use crate::ast::TypeDef;
    use crate::codegen::{empty_test_ctx, test_module};

    fn product(name: &str, field: &str, ty: &str) -> TypeDef {
        TypeDef::Product {
            name: name.to_string(),
            fields: vec![(field.to_string(), ty.to_string())],
            line: 1,
        }
    }

    #[test]
    fn a_bare_name_never_resolves_outside_what_the_emitted_module_depends_on() {
        // The fuel emitter reads a return type as text, so a named type
        // reaches this walk as a bare name. `Bodies` means the `Progress` it
        // imports; `Unrelated` declares that name and is in nobody's
        // `depends`. jasisz/aver#1134.
        let mut ctx = empty_test_ctx();
        ctx.type_defs = vec![product("Progress", "entry", "Bool")];
        ctx.modules = vec![
            test_module("Unrelated", &[], vec![product("Progress", "tag", "String")]),
            test_module("Snapshot", &[], vec![product("Progress", "done", "Int")]),
            test_module(
                "Bodies",
                &["Snapshot"],
                vec![product("Walk", "step", "Progress")],
            ),
        ];

        let found =
            ctx.with_module_scope(Some("Bodies"), || find_type_def(&ctx, "Progress").cloned());
        let TypeDef::Product { fields, .. } = found.expect("Bodies can see Snapshot.Progress")
        else {
            panic!("expected a product type");
        };
        assert_eq!(
            fields[0].0, "done",
            "a bare name must resolve inside what Bodies depends on, and never to the entry"
        );
    }

    #[test]
    fn the_emitted_module_prefers_its_own_declaration() {
        let mut ctx = empty_test_ctx();
        ctx.modules = vec![
            test_module("Snapshot", &[], vec![product("Progress", "done", "Int")]),
            test_module(
                "Bodies",
                &["Snapshot"],
                vec![product("Progress", "ratio", "Float")],
            ),
        ];

        let found =
            ctx.with_module_scope(Some("Bodies"), || find_type_def(&ctx, "Progress").cloned());
        let TypeDef::Product { fields, .. } = found.expect("Bodies declares Progress itself")
        else {
            panic!("expected a product type");
        };
        assert_eq!(fields[0].0, "ratio");
    }

    #[test]
    fn the_entry_still_sees_its_own_types_first() {
        // No active scope means the entry is being emitted, and its own
        // declarations keep answering before any dependency's.
        let mut ctx = empty_test_ctx();
        ctx.type_defs = vec![product("Progress", "entry", "Bool")];
        ctx.modules = vec![test_module(
            "Snapshot",
            &[],
            vec![product("Progress", "done", "Int")],
        )];

        let TypeDef::Product { fields, .. } =
            find_type_def(&ctx, "Progress").expect("the entry declares Progress")
        else {
            panic!("expected a product type");
        };
        assert_eq!(fields[0].0, "entry");
    }
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

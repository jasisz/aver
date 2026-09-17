//! Functional induction for explanations with an existing checked recursion measure.
//! Recursion contracts and kernel-generated equations remain the source of truth.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{Expr, FnDef, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::lean::expr::{aver_name_to_lean, emit_expr, resolve_rewrite_output};
use crate::codegen::{CodegenContext, common};

pub(super) fn list_measure<'a>(fd: &FnDef, ctx: &'a CodegenContext) -> Option<&'a str> {
    match common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()) {
        Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne { param },
        }) => Some(param),
        _ => None,
    }
}

pub(super) fn callee<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let (name, _) = super::super::shared::call_name_args(expr)?;
    let id = ctx.symbol_table.resolve_fn_id_in(&name, scope)?;
    let key = &ctx.symbol_table.fn_entry(id).key;
    ctx.fn_def_by_name(&key.name, key.scope_str())
}

pub(super) fn lean_name(fd: &FnDef, ctx: &CodegenContext) -> String {
    match common::fn_owning_scope_for(ctx, fd) {
        Some(scope) => format!(
            "{}.{}",
            aver_name_to_lean(scope),
            aver_name_to_lean(&fd.name)
        ),
        None => super::super::shared::entry_qualified_lean_name(ctx, &fd.name),
    }
}

/// Prove map/suffix properties locally rather than trusting the function's
/// shape. A length-changing function makes this candidate fail normally.
/// A single underscore keeps these hypotheses visible to `grind`; Lean marks
/// double-underscore hypothesis names as implementation details and skips them.
pub(super) fn checked_map_lemmas(names: &[String]) -> String {
    let mut proofs = String::new();
    for (index, name) in names.iter().enumerate() {
        let length = format!("_aver_transport_length_{index}");
        let drop = format!("_aver_transport_drop_{index}");
        proofs.push_str(&format!(
            "have {length} : ∀ xs, List.length ({name} xs) = List.length xs := (by intro xs; induction xs with | nil => simp only [{name}, List.length_nil] | cons x xs ih => simpa only [{name}, List.length_cons] using congrArg Nat.succ ih); have {drop} : ∀ xs n, {name} (List.drop n xs) = List.drop n ({name} xs) := (by intro xs n; induction xs generalizing n with | nil => simp only [{name}, List.drop_nil] | cons x xs ih => cases n with | zero => rfl | succ n => simpa only [{name}, List.drop_succ_cons] using ih n); "
        ));
    }
    proofs
}

pub(super) fn plan(
    vb: &VerifyBlock,
    index: usize,
    expr: &Spanned<Expr>,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<String> {
    let scope = ctx.active_module_scope();
    let fd = callee(expr, ctx, scope.as_deref())?;
    if !fd.effects.is_empty() {
        return None;
    }
    // LawLower records the canonical source call and its checked input once.
    // Reuse the plan for this separately checked explanation;
    // backend-specific functional/measure tactics still prove every branch.
    if let Some(id) = ctx.law_target_fn_id(&vb.fn_name)
        && let Some(shared) = ctx
            .proof_ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == id && t.law_name == law.name)
            .and_then(|t| t.reason_inductions.get(index))
            .and_then(Option::as_ref)
        && shared.source_call.node == resolve_rewrite_output(expr, ctx, None).node
    {
        let call = emit_expr(&shared.source_call, ctx);
        return Some(match shared.measure {
            crate::ir::proof_ir::LawInductionMeasure::NonnegativeInt => {
                format!("fun_induction {call}")
            }
            crate::ir::proof_ir::LawInductionMeasure::SequenceLength => {
                let others = law
                    .givens
                    .iter()
                    .filter(|g| g.name != shared.driver)
                    .map(|g| aver_name_to_lean(&g.name))
                    .collect::<Vec<_>>();
                let generalizing = if others.is_empty() {
                    String::new()
                } else {
                    format!(" generalizing {}", others.join(" "))
                };
                format!(
                    "first | fun_induction {call} | (induction {} using (measure List.length).wf.induction{generalizing} <;> dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at * <;> rw [{}.eq_def])",
                    aver_name_to_lean(&shared.driver),
                    lean_name(fd, ctx)
                )
            }
        });
    }
    let measure = list_measure(fd, ctx);
    let native_integer = matches!(
        common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
        Some(crate::ir::RecursionContract::WellFoundedToNat { .. })
    );
    if measure.is_none() && !native_integer {
        return None;
    }
    let Expr::FnCall(_, args) = &expr.node else {
        return None;
    };
    // Composite or repeated arguments would require retaining their equalities
    // while generalizing. Keep that outside this first structural rung.
    let mut seen = HashSet::new();
    for arg in args {
        let name = match &arg.node {
            Expr::Ident(name) | Expr::Resolved { name, .. } => name,
            _ => return None,
        };
        if !law.givens.iter().any(|g| g.name == *name) || !seen.insert(name) {
            return None;
        }
    }
    if native_integer {
        let call = emit_expr(&resolve_rewrite_output(expr, ctx, None), ctx);
        return Some(format!("fun_induction {call}"));
    }
    let measure = measure?;
    let position = fd.params.iter().position(|(name, _)| name == measure)?;
    let (Expr::Ident(argument) | Expr::Resolved { name: argument, .. }) = &args.get(position)?.node
    else {
        return None;
    };
    let others = law
        .givens
        .iter()
        .filter(|g| g.name != *argument)
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>();
    let generalizing = if others.is_empty() {
        String::new()
    } else {
        format!(" generalizing {}", others.join(" "))
    };
    let call = emit_expr(&resolve_rewrite_output(expr, ctx, None), ctx);
    // Lean can fail to construct a functional principle for a local match.
    // The checked length measure supplies an IH for every shorter list,
    // including computed slices; reduce its relation before solving leaves.
    Some(format!(
        "first | fun_induction {call} | (induction {} using (measure List.length).wf.induction{generalizing} <;> dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at * <;> rw [{}.eq_def])",
        aver_name_to_lean(argument),
        lean_name(fd, ctx)
    ))
}

/// Walk only this law's calls, resolving every edge in its owner's scope.
/// Unsupported recursive functions stay opaque; no fuel equation is imported.
pub(super) struct Definitions {
    /// Checked list-recursive equations for a structural step, without
    /// expanding the implementations summarized by cited transition laws.
    pub(super) list_steps: String,
    pub(super) list_maps: String,
    pub(super) unary_list_maps: Vec<String>,
    pub(super) completed: String,
    pub(super) heads: String,
    /// Equations of outer calls and their direct arguments, without the full cone.
    pub(super) head_equations: String,
    pub(super) structural_reason: bool,
    pub(super) simp: String,
    pub(super) grind: String,
    pub(super) unfold_once: Vec<(String, bool)>,
    /// The cone, the claim, the guard or an explanation calls `Map.set`, so
    /// the solver cites the prelude's facts about it. The call counts wherever
    /// it sits, including inside a field of a record the cone rebuilds.
    pub(super) map_facts: bool,
    /// The same for `Map.remove`: a branch that drops one entry needs the
    /// removal's own size fact, which the `set` family does not carry.
    pub(super) map_remove_facts: bool,
}

pub(super) fn definitions(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Definitions {
    let scope = ctx.active_module_scope();
    let cone = ctx
        .law_target_fn_id(&vb.fn_name)
        .and_then(|id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == id && t.law_name == law.name)
        })
        .map(|t| t.function_cone.as_slice())
        .unwrap_or(&[]);
    let seen: HashSet<_> = cone.iter().copied().collect();
    let mut out = BTreeMap::new();
    let mut list_steps = BTreeSet::new();
    let mut list_maps = BTreeSet::new();
    let mut unary_list_maps = BTreeSet::new();
    let mut completed = BTreeSet::new();
    let mut unfold_once = Vec::new();
    let law_calls = |builtin: &str| {
        law.because
            .iter()
            .chain(law.when.iter())
            .chain([&law.lhs, &law.rhs])
            .any(|expr| super::super::shared::expr_calls_builtin(expr, builtin))
    };
    let mut map_facts = law_calls("Map.set");
    let mut map_remove_facts = law_calls("Map.remove");
    for &id in cone {
        let key = &ctx.symbol_table.fn_entry(id).key;
        let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str()) else {
            continue;
        };
        map_facts |= super::super::shared::fn_body_calls_builtin(fd, "Map.set");
        map_remove_facts |= super::super::shared::fn_body_calls_builtin(fd, "Map.remove");
        let recursive = ctx.recursive_fns.contains(&id);
        if list_measure(fd, ctx).is_some() {
            list_steps.insert(lean_name(fd, ctx));
            // A terminal constructor may also occur on the left after a
            // splice. Its checked equation reduces that one boundary without
            // unfolding another recursive call on an unknown outcome.
            if super::composition::first_constructor_branch(fd) {
                completed.insert(format!("{}.eq_1", lean_name(fd, ctx)));
            }
            if fd.return_type.starts_with("List<") {
                list_maps.insert(lean_name(fd, ctx));
                if fd.params.len() == 1 {
                    unary_list_maps.insert(lean_name(fd, ctx));
                }
            }
        }
        // Subtractive countdown equations expose fixed-width steps. Keep
        // floor-division recursion opaque: its equations recursively grow
        // the arithmetic search even when cited laws already summarize it.
        let subtractive = matches!(
            common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
            Some(crate::ir::RecursionContract::WellFoundedToNat {
                floor_div: None,
                ..
            })
        );
        if matches!(
            common::find_fn_contract_for_fn(ctx, fd).and_then(|c| c.recursion.as_ref()),
            Some(
                crate::ir::RecursionContract::WellFoundedToNat {
                    floor_div: Some(_),
                    ..
                } | crate::ir::RecursionContract::WellFoundedSequenceGap { .. }
            )
        ) {
            unfold_once.push(lean_name(fd, ctx));
        }
        if !recursive || list_measure(fd, ctx).is_some() || subtractive {
            out.insert(lean_name(fd, ctx), recursive);
        }
    }
    // A mutual helper's original equation is available only when the same
    // checked measure that emits its native definition succeeds. Fuel remains opaque.
    common::route_pure_components_per_scope(
        ctx,
        |fd| fd.effects.is_empty(),
        |fns, _| {
            if fns.len() > 1
                && fns
                    .iter()
                    .any(|fd| common::fn_id_for_decl(ctx, fd).is_some_and(|id| seen.contains(&id)))
                && crate::codegen::lean::toplevel::emit_native_mutual_group(fns, ctx).is_some()
            {
                for fd in fns {
                    if common::fn_id_for_decl(ctx, fd).is_some_and(|id| seen.contains(&id)) {
                        out.insert(format!("= {}.eq_def", lean_name(fd, ctx)), true);
                    }
                }
            }
            Vec::new()
        },
    );
    // An induction hypothesis says reason(rest) = true. Its match equation
    // reveals the checked facts even when `rest` is not a known constructor.
    for reason in &law.because {
        if let Some(fd) = callee(reason, ctx, scope.as_deref())
            && list_measure(fd, ctx).is_some()
        {
            out.insert(format!("= {}.eq_def", lean_name(fd, ctx)), true);
        }
    }
    // Only outer calls: retain computations passed as arguments as opaque terms.
    let heads = law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .filter_map(|expr| callee(expr, ctx, scope.as_deref()))
        .filter(|fd| {
            fd.effects.is_empty()
                && common::fn_id_for_decl(ctx, fd)
                    .is_some_and(|id| !ctx.recursive_fns.contains(&id))
        })
        .map(|fd| lean_name(fd, ctx))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .join(", ");
    // Explanations can expose a conclusion through an immediate argument
    // adapter (e.g. encode(decode(item))). Keep deeper implementation calls
    // opaque, and leave ordinary citation-only laws on their existing route.
    let mut head_seen = HashSet::new();
    let mut head_equations = law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .filter(|_| !law.because.is_empty())
        .flat_map(|expr| {
            let mut calls = vec![expr];
            crate::codegen::expr_walk::for_each_child(expr, &mut |child| calls.push(child));
            calls
        })
        .filter_map(|expr| callee(expr, ctx, scope.as_deref()))
        .map(|fd| lean_name(fd, ctx))
        .filter(|name| out.contains_key(name) || out.contains_key(&format!("= {name}.eq_def")))
        .filter(|name| head_seen.insert(name.clone()))
        .map(|name| format!("= {name}.eq_def"))
        .collect::<Vec<_>>();
    // Register explanation equations before the goal's adapters: this lets
    // matching expose known facts before expanding the terms they describe.
    if !head_equations.is_empty() {
        if law_calls("List.concat") {
            head_equations.extend([
                "List.cons_append".to_string(),
                "List.nil_append".to_string(),
            ]);
        }
        head_equations.push("List.reverse_reverse".to_string());
    }
    Definitions {
        list_steps: list_steps.into_iter().collect::<Vec<_>>().join(", "),
        list_maps: list_maps.into_iter().collect::<Vec<_>>().join(", "),
        unary_list_maps: unary_list_maps.into_iter().collect(),
        completed: completed.into_iter().collect::<Vec<_>>().join(", "),
        heads,
        head_equations: head_equations.join(", "),
        structural_reason: law.because.iter().any(|reason| {
            callee(reason, ctx, scope.as_deref()).is_some_and(|fd| list_measure(fd, ctx).is_some())
        }),
        map_facts,
        map_remove_facts,
        unfold_once: unfold_once
            .into_iter()
            .map(|name| {
                let reason = law.because.iter().any(|expr| {
                    callee(expr, ctx, scope.as_deref()).is_some_and(|fd| lean_name(fd, ctx) == name)
                });
                (name, reason)
            })
            .collect(),
        simp: out
            .iter()
            .filter(|(_, recursive)| !**recursive)
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", "),
        grind: out
            .into_iter()
            .map(|(name, recursive)| {
                if recursive {
                    name
                } else {
                    format!("= {name}.eq_def")
                }
            })
            .collect::<Vec<_>>()
            .join(", "),
    }
}

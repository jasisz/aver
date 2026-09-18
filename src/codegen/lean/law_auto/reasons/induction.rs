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

/// Follow transparent outer wrappers to the fold being compared.
pub(super) fn outer_fold<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let mut fd = callee(expr, ctx, scope)?;
    let mut seen = HashSet::new();
    loop {
        if !seen.insert(lean_name(fd, ctx)) {
            return None;
        }
        if list_measure(fd, ctx).is_some() {
            return Some(fd);
        }
        let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        fd = callee(body, ctx, common::fn_owning_scope_for(ctx, fd))?;
    }
}

/// A finite prefix can surround the compared fold with matches instead of
/// a direct wrapper call. Select its unique direct recursive boundary.
pub(super) fn body_fold<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let fd = callee(expr, ctx, scope)?;
    let mut folds = BTreeMap::new();
    for stmt in fd.body.stmts() {
        let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
        crate::codegen::expr_walk::walk(expr, &mut |expr| {
            if let Some(called) = callee(expr, ctx, common::fn_owning_scope_for(ctx, fd))
                && list_measure(called, ctx).is_some()
                && !called.return_type.starts_with("List<")
            {
                folds.insert(lean_name(called, ctx), called);
            }
        });
    }
    (folds.len() == 1).then(|| *folds.values().next().unwrap())
}

/// A shared computed record can stay opaque; state constructors and scalar
/// routers still need to reduce so the fold's next case becomes visible.
fn constructs_result_record(fd: &FnDef) -> bool {
    fd.body.stmts().iter().any(|stmt| {
        let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
        crate::codegen::expr_walk::any(expr, &mut |expr| {
            matches!(&expr.node,
            Expr::RecordCreate { type_name, .. } if type_name == &fd.return_type)
        })
    })
}

/// Common finite record calls can remain opaque while the recursive steps align.
pub(super) fn direct_finite_calls(fd: &FnDef, ctx: &CodegenContext) -> BTreeSet<String> {
    let mut calls = BTreeSet::new();
    for stmt in fd.body.stmts() {
        let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
        crate::codegen::expr_walk::walk(expr, &mut |expr| {
            if let Some(called) = callee(expr, ctx, common::fn_owning_scope_for(ctx, fd))
                && called.effects.is_empty()
                && constructs_result_record(called)
                && common::fn_id_for_decl(ctx, called)
                    .is_some_and(|id| !ctx.recursive_fns.contains(&id))
            {
                calls.insert(lean_name(called, ctx));
            }
        });
    }
    calls
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
pub(super) fn checked_map_lemmas(names: &[String], slices_first: bool) -> String {
    let mut proofs = String::new();
    for (index, name) in names.iter().enumerate() {
        let length = format!("_aver_transport_length_{index}");
        let drop = format!("_aver_transport_drop_{index}");
        let (drop_prop, orient) = if slices_first {
            (
                format!("List.drop n ({name} xs) = {name} (List.drop n xs)"),
                "symm; ",
            )
        } else {
            (
                format!("{name} (List.drop n xs) = List.drop n ({name} xs)"),
                "",
            )
        };
        proofs.push_str(&format!(
            "have {length} : ∀ xs, List.length ({name} xs) = List.length xs := (by intro xs; induction xs with | nil => simp only [{name}, List.length_nil] | cons x xs ih => simpa only [{name}, List.length_cons] using congrArg Nat.succ ih); have {drop} : ∀ xs n, {drop_prop} := (by intro xs n; {orient}induction xs generalizing n with | nil => simp only [{name}, List.drop_nil] | cons x xs ih => cases n with | zero => rfl | succ n => simpa only [{name}, List.drop_succ_cons] using ih n); "
        ));
    }
    proofs
}

/// Select one-output-per-cell recursions for the locally checked map facts.
/// Other list observers (notably reversal) need their own proof strategy.
pub(super) fn is_unary_list_map(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 1 {
        return false;
    }
    let [crate::ast::Stmt::Expr(expr)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { arms, .. } = &expr.node else {
        return false;
    };
    let [nil, cons] = arms.as_slice() else {
        return false;
    };
    let crate::ast::Pattern::Cons(_, tail) = &cons.pattern else {
        return false;
    };
    if !matches!(nil.pattern, crate::ast::Pattern::EmptyList)
        || !matches!(&nil.body.node, Expr::List(items) if items.is_empty())
    {
        return false;
    }
    let Some((name, args)) = super::super::shared::call_name_args(&cons.body) else {
        return false;
    };
    if name != "List.prepend" || args.len() != 2 {
        return false;
    }
    let Some(recursive) = callee(&args[1], ctx, common::fn_owning_scope_for(ctx, fd)) else {
        return false;
    };
    if common::fn_id_for_decl(ctx, recursive) != common::fn_id_for_decl(ctx, fd) {
        return false;
    }
    let Expr::FnCall(_, values) = &args[1].node else {
        return false;
    };
    matches!(values.as_slice(), [value] if matches!(&value.node, Expr::Ident(name) | Expr::Resolved { name, .. } if name == tail))
}

/// Equations for visible cells of a two-arm list map; arbitrary tails stay opaque.
pub(super) fn map_constructor_equations(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    let [crate::ast::Stmt::Expr(expr)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { arms, .. } = &expr.node else {
        return None;
    };
    if arms.len() != 2
        || !matches!(arms[0].pattern, crate::ast::Pattern::EmptyList)
        || !matches!(arms[1].pattern, crate::ast::Pattern::Cons(..))
    {
        return None;
    }
    let name = lean_name(fd, ctx);
    Some(format!("= {name}.eq_1, = {name}.eq_2"))
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

/// A fold can consume a finite prefix before its recursive call. Such a
/// step needs staged normalization even when its final tail is structural.
fn nested_list_matches(fd: &FnDef) -> bool {
    fn visit(expr: &Spanned<Expr>, depth: usize) -> bool {
        let depth = depth
            + usize::from(matches!(&expr.node, Expr::Match { arms, .. }
            if arms.iter().any(|arm| matches!(arm.pattern, crate::ast::Pattern::Cons(..)))));
        if depth > 1 {
            return true;
        }
        let mut nested = false;
        crate::codegen::expr_walk::for_each_child(expr, &mut |child| nested |= visit(child, depth));
        nested
    }
    fd.body.stmts().iter().any(|stmt| {
        let (crate::ast::Stmt::Expr(expr) | crate::ast::Stmt::Binding(_, _, expr)) = stmt;
        visit(expr, 0)
    })
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
    /// A checked fold consumes an observed prefix or recurs on a computed suffix.
    pub(super) staged_recursion: bool,
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
    let mut staged_recursion = false;
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
            staged_recursion |=
                crate::codegen::recursion::detect::single_list_structural_param_index(fd).is_none()
                    || (super::composition::first_constructor_branch(fd)
                        && nested_list_matches(fd));
            list_steps.insert(lean_name(fd, ctx));
            // A terminal constructor may also occur on the left after a
            // splice. Its checked equation reduces that one boundary without
            // unfolding another recursive call on an unknown outcome.
            if super::composition::first_constructor_branch(fd) {
                completed.insert(format!("{}.eq_1", lean_name(fd, ctx)));
            }
            if fd.return_type.starts_with("List<") {
                list_maps.insert(lean_name(fd, ctx));
                if is_unary_list_map(fd, ctx) {
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
        staged_recursion,
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

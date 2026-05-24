/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod arithmetic;
mod induction;
mod sampled;
mod shared;
mod spec;

use super::VerifyEmitMode;
use super::expr::{aver_name_to_lean, emit_expr};
use crate::ast::{Expr, Literal, MatchArm, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::{collect_missing_helper_law_hints, missing_helper_law_message};
use sampled::emit_guarded_domain_law;

pub struct AutoProof {
    pub support_lines: Vec<String>,
    pub proof_lines: Vec<String>,
    /// When true, the main theorem statement is already included in `support_lines`
    /// and should not be emitted separately by the caller.
    pub replaces_theorem: bool,
}

/// Look up the strategy `proof_lower::populate_law_theorems` pinned
/// on `(fn_name, law_name)`. Returns `None` when no contract was
/// lowered (LawLower disabled or the verify block wasn't a Law).
fn law_strategy_for(
    ctx: &CodegenContext,
    fn_name: &str,
    law_name: &str,
) -> Option<crate::ir::ProofStrategy> {
    ctx.proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == fn_name && t.law_name == law_name)
        .map(|t| t.strategy.clone())
}

pub fn emit_verify_law_forall_auto_proof(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    theorem_base: &str,
    quant_params: &str,
    theorem_prop: &str,
) -> Option<AutoProof> {
    if verify_mode != VerifyEmitMode::NativeDecide {
        return None;
    }

    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let proof_intro_names = extend_intro_names_with_premises(law, &intro_names);

    // Structural induction — IR-pinned `ProofStrategy::Induction`
    // wins first. The legacy chain ran induction at this position
    // unconditionally; the IR-pin path keeps that priority while
    // making the decision visible in `proof_ir.law_theorems`.
    // Falls through to the legacy emit_structural_induction_law
    // (still called for BackendDispatch laws that the lowerer
    // hasn't classified — shouldn't trigger for canonical recursive-
    // ADT shapes after Step 31).
    if matches!(
        law_strategy_for(ctx, &vb.fn_name, &law.name),
        Some(crate::ir::ProofStrategy::Induction { .. })
    ) && let Some(proof) = induction::emit_structural_induction_law(
        vb,
        law,
        ctx,
        &intro_names,
        theorem_base,
        quant_params,
        theorem_prop,
    ) {
        return Some(proof);
    }
    // IR-pinned strategies. The lowerer's decision wins over the
    // ad-hoc detection chain that follows; backend just renders the
    // tactic the IR selected. Each variant has a fixed Lean shape;
    // the IR's `BinOp` payload maps to a specific lemma name here.
    if let Some(strategy) = law_strategy_for(ctx, &vb.fn_name, &law.name) {
        use crate::ast::BinOp;
        use crate::ir::ProofStrategy;
        let fn_lean = aver_name_to_lean(&vb.fn_name);
        let proof_lines = match strategy {
            ProofStrategy::Reflexive => Some(vec!["rfl".to_string()]),
            ProofStrategy::Commutative { op } => match op {
                BinOp::Add => Some(vec![format!("simp [{}, Int.add_comm]", fn_lean)]),
                BinOp::Mul => Some(vec![format!("simp [{}, Int.mul_comm]", fn_lean)]),
                _ => None,
            },
            ProofStrategy::Associative { op } => match op {
                BinOp::Add => Some(vec![format!("simp [{}, Int.add_assoc]", fn_lean)]),
                BinOp::Mul => Some(vec![format!("simp [{}, Int.mul_assoc]", fn_lean)]),
                _ => None,
            },
            ProofStrategy::IdentityElement { .. } => {
                // Add → `simp [fn]` collapses `a + 0` / `0 + a`;
                // Mul → same against `a * 1` / `1 * a`; Sub →
                // `simp [fn]` reduces `a - 0` to `a` (one-sided —
                // detector enforces shape). Op-agnostic emit:
                // unfold the wrapper and simp closes via Lean's
                // built-in identity lemmas.
                Some(vec![format!("simp [{}]", fn_lean)])
            }
            ProofStrategy::UnaryEqualsBinary { ref inner_fn } => {
                // `outer(a) = inner(a, K)` (or `inner(K, a)`) —
                // simp unfolds both fns to the same underlying op
                // expression on each side.
                Some(vec![format!(
                    "simp [{}, {}]",
                    fn_lean,
                    aver_name_to_lean(inner_fn)
                )])
            }
            ProofStrategy::AntiCommutative { neg_on_rhs, .. } => {
                // `Int.neg_sub b a : -(b - a) = a - b`. `.symm` flip
                // when the user's law puts the negation on rhs.
                let a = aver_name_to_lean(&law.givens[0].name);
                let b = aver_name_to_lean(&law.givens[1].name);
                let step = if neg_on_rhs {
                    format!("simpa [{}] using (Int.neg_sub {} {}).symm", fn_lean, b, a)
                } else {
                    format!("simpa [{}] using (Int.neg_sub {} {})", fn_lean, b, a)
                };
                Some(vec![step])
            }
            // LinearArithmetic runs at its position in the chain
            // (below spec_equivalence + maps) — falls through here
            // and emits in the dedicated arm further down.
            ProofStrategy::LinearArithmetic { .. } => None,
            _ => None,
        };
        if let Some(lines) = proof_lines {
            return Some(AutoProof {
                support_lines: Vec::new(),
                proof_lines: intro_then(&proof_intro_names, lines),
                replaces_theorem: false,
            });
        }
    }

    arithmetic::emit_binary_wrapper_law(vb, law, ctx, &proof_intro_names)
        .map(|proof_lines| AutoProof {
            support_lines: Vec::new(),
            proof_lines,
            replaces_theorem: false,
        })
        .or_else(|| {
            arithmetic::emit_unary_wrapper_equivalence_law(vb, law, ctx, &proof_intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
        .or_else(|| spec::emit_spec_function_equivalence_law(vb, law, ctx, &proof_intro_names))
        .or_else(|| {
            // IR-pinned Map library axiom (has_set_self / get_set_self).
            // The lowerer detected the canonical shape and captured the
            // (m, k, v) args; backend just renders the Lean simpa.
            if let Some(crate::ir::ProofStrategy::LibraryAxiom {
                ref axiom,
                ref args,
            }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
                && matches!(axiom.as_str(), "Map.has_set_self" | "Map.get_set_self")
                && args.len() == 3
            {
                let lemma = match axiom.as_str() {
                    "Map.has_set_self" => "AverMap.has_set_self",
                    "Map.get_set_self" => "AverMap.get_set_self",
                    _ => unreachable!(),
                };
                let atom_arg = |e: &crate::ast::Spanned<crate::ast::Expr>| {
                    let rendered = emit_expr(e, ctx);
                    if rendered.contains(' ') && !rendered.starts_with('(') {
                        format!("({rendered})")
                    } else {
                        rendered
                    }
                };
                return Some(AutoProof {
                    support_lines: Vec::new(),
                    proof_lines: intro_then(
                        &proof_intro_names,
                        vec![format!(
                            "simpa using {} {} {} {}",
                            lemma,
                            atom_arg(&args[0]),
                            atom_arg(&args[1]),
                            atom_arg(&args[2]),
                        )],
                    ),
                    replaces_theorem: false,
                });
            }
            None
        })
        .or_else(|| {
            // IR-pinned `MapUpdatePostcondition` — the lowerer
            // validated the outer fn's "inspect get, set in every
            // arm" body shape and captured the law's (map, key)
            // args + the helper-fn unfold set. Backend renders the
            // 2-line `simp [outer (, extras)] ; cases h : AverMap.get
            // m k <;> simp [AverMap.<axiom> (, extras)]` tactic.
            if let Some(crate::ir::ProofStrategy::MapUpdatePostcondition {
                ref outer_fn,
                kind,
                ref map_arg,
                ref key_arg,
                ref extra_unfolds,
            }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
            {
                let outer_lean = aver_name_to_lean(outer_fn);
                let extras_lean: Vec<String> =
                    extra_unfolds.iter().map(|n| aver_name_to_lean(n)).collect();
                let atom_render = |e: &crate::ast::Spanned<crate::ast::Expr>| {
                    let rendered = emit_expr(e, ctx);
                    if rendered.contains(' ') && !rendered.starts_with('(') {
                        format!("({rendered})")
                    } else {
                        rendered
                    }
                };
                let (axiom_lemma, prefix_extras): (&str, Vec<String>) = match kind {
                    crate::ir::MapUpdatePostconditionKind::HasAfter => {
                        ("AverMap.has_set_self", Vec::new())
                    }
                    crate::ir::MapUpdatePostconditionKind::GetAfter => {
                        ("AverMap.get_set_self", extras_lean.clone())
                    }
                };
                let simp_first: String = {
                    let mut items = vec![outer_lean.clone()];
                    items.extend(prefix_extras);
                    format!("simp [{}]", items.join(", "))
                };
                let simp_second: String = {
                    let mut items = vec![axiom_lemma.to_string()];
                    if matches!(kind, crate::ir::MapUpdatePostconditionKind::GetAfter) {
                        items.push(outer_lean.clone());
                        items.extend(extras_lean.iter().cloned());
                    }
                    format!(
                        "cases h : AverMap.get {} {} <;> simp [{}]",
                        atom_render(map_arg),
                        atom_render(key_arg),
                        items.join(", ")
                    )
                };
                return Some(AutoProof {
                    support_lines: Vec::new(),
                    proof_lines: intro_then(&proof_intro_names, vec![simp_first, simp_second]),
                    replaces_theorem: false,
                });
            }
            None
        })
        .or_else(|| {
            // IR-pinned `MapKeyTrackedIncrement` — the lowerer
            // validated the outer fn's "tracked counter" body
            // template (Some(n) -> n + 1, None -> 1) and matched the
            // law against the `Option.withDefault`-defaulted shape.
            // Backend renders the 2-line `simp [outer] ; cases h :
            // AverMap.get m k <;> simp [AverMap.get_set_self, h]`
            // tactic.
            if let Some(crate::ir::ProofStrategy::MapKeyTrackedIncrement {
                ref outer_fn,
                ref map_arg,
                ref key_arg,
            }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
            {
                let outer_lean = aver_name_to_lean(outer_fn);
                let atom_render = |e: &crate::ast::Spanned<crate::ast::Expr>| {
                    let rendered = emit_expr(e, ctx);
                    if rendered.contains(' ') && !rendered.starts_with('(') {
                        format!("({rendered})")
                    } else {
                        rendered
                    }
                };
                let lines = vec![
                    format!("simp [{}]", outer_lean),
                    format!(
                        "cases h : AverMap.get {} {} <;> simp [AverMap.get_set_self, h]",
                        atom_render(map_arg),
                        atom_render(key_arg),
                    ),
                ];
                return Some(AutoProof {
                    support_lines: Vec::new(),
                    proof_lines: intro_then(&proof_intro_names, lines),
                    replaces_theorem: false,
                });
            }
            None
        })
        .or_else(|| {
            // IR-pinned SimpOmegaUnfold takes precedence over the
            // legacy detection here — the lowerer already ran the
            // same shape check and captured `unfold_fns`,
            // `wrapper_return`, `smart_guard`. When the IR didn't
            // pin (BackendDispatch), fall through to the legacy
            // detector below.
            if let Some(crate::ir::ProofStrategy::LinearArithmetic {
                ref unfold_fns,
                wrapper_return,
                ref smart_guard,
                lifted,
            }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
            {
                // Lifted laws use base intro names — the Subtype
                // lift incorporates the `when` premise into the
                // theorem's quantifier types, so the user-side
                // hypotheses (`h_a`, `h_b`, `h_when`) aren't
                // available in the proof goal. Non-lifted paths
                // keep premise expansion for by_cases hypotheses.
                let chosen_intro: &[String] = if lifted {
                    &intro_names
                } else {
                    &proof_intro_names
                };
                return Some(AutoProof {
                    support_lines: Vec::new(),
                    proof_lines: emit_simp_omega_from_ir(
                        unfold_fns,
                        wrapper_return,
                        smart_guard.as_ref(),
                        lifted,
                        chosen_intro,
                        ctx,
                    ),
                    replaces_theorem: false,
                });
            }
            emit_simp_omega_law(vb, law, ctx, &proof_intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
        .or_else(|| {
            emit_guarded_domain_law(law).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
}

/// Try `simp [fn_names...] ; omega` for laws on Int-domain functions.
///
/// Works when the function is a non-recursive match on Int args
/// (e.g. `computeScore(0, level) => 0`). `simp` unfolds the function,
/// `omega` closes the linear arithmetic goal.
/// Render the simp+omega tactic from IR-pinned data. Mirrors the
/// emit body of the legacy `emit_simp_omega_law` (kept as fallback
/// for `BackendDispatch`) but sources `unfold_fns` / `wrapper_
/// return` / `smart_guard` from `ProofIR.law_theorems[*].strategy`.
fn emit_simp_omega_from_ir(
    unfold_fns: &[String],
    wrapper_return: bool,
    smart_guard: Option<&crate::ir::SmartGuard>,
    lifted: bool,
    intro_names: &[String],
    ctx: &CodegenContext,
) -> Vec<String> {
    let lean_names: Vec<String> = unfold_fns.iter().map(|n| aver_name_to_lean(n)).collect();
    if lifted && wrapper_return {
        // Subtype/subset lift carries the smart-constructor
        // invariant in the type — the law-quantified vars are
        // already `Natural` (etc.) in the theorem statement, so
        // by_cases case-split is unnecessary. Plain unfold + simp
        // with arithmetic lemmas closes via Lean's built-in
        // commutativity normalisation.
        intro_then(
            intro_names,
            vec![
                format!("unfold {}", lean_names.join(" ")),
                "simp [Int.add_comm, Int.mul_comm]".to_string(),
            ],
        )
    } else if wrapper_return {
        let by_cases_clauses: Vec<String> = intro_names
            .iter()
            .map(|n| {
                let predicate = match smart_guard {
                    Some(g) => {
                        let substituted = crate::codegen::common::substitute_ident_in_expr(
                            &g.predicate,
                            &g.param,
                            n,
                        );
                        emit_expr(&substituted, ctx)
                    }
                    None => format!("{n} ≥ 0"),
                };
                format!("by_cases h_{n} : {predicate}")
            })
            .collect();
        let by_cases_chain = by_cases_clauses.join(" <;> ");
        let simp_hyps: Vec<String> = intro_names
            .iter()
            .map(|n| format!("h_{n}"))
            .chain(["Int.add_comm".to_string(), "Int.mul_comm".to_string()])
            .collect();
        let simp_args = simp_hyps.join(", ");
        intro_then(
            intro_names,
            vec![
                format!("unfold {}", lean_names.join(" ")),
                format!("{by_cases_chain} <;> simp [{simp_args}]"),
            ],
        )
    } else {
        intro_then(
            intro_names,
            vec![format!("simp only [{}] <;> omega", lean_names.join(", "))],
        )
    }
}

fn emit_simp_omega_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    // Only attempt when all givens are Int.
    if law.givens.is_empty() || !law.givens.iter().all(|g| g.type_name == "Int") {
        return None;
    }
    // Collect all user-defined function names referenced in lhs and rhs.
    let mut fn_names = std::collections::BTreeSet::new();
    collect_fn_calls(&law.lhs, &mut fn_names);
    collect_fn_calls(&law.rhs, &mut fn_names);
    fn_names.insert(vb.fn_name.clone());
    // Transitively expand: include functions called from the body
    // of every fn we've already collected. Without this, a law on
    // `safeSum(a, b) => safeSum(b, a)` collects only `safeSum`
    // itself — but the unfold tactic needs every function that
    // appears under it (including cross-module callees like
    // `Modules.Natural.Natural.fromInt`) so the resulting goal is
    // free of opaque match-on-Except branches that `simp` can't
    // close. Bounded iteration: each round can only add what's
    // reachable from the new set, so it converges in O(items).
    loop {
        let before = fn_names.len();
        let snapshot: Vec<String> = fn_names.iter().cloned().collect();
        for item in &ctx.items {
            if let crate::ast::TopLevel::FnDef(fd) = item
                && snapshot.contains(&fd.name)
            {
                for stmt in fd.body.stmts() {
                    match stmt {
                        crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                            collect_fn_calls(e, &mut fn_names);
                        }
                    }
                }
            }
        }
        if fn_names.len() == before {
            break;
        }
    }
    // Only proceed if all referenced functions exist in ctx, are non-recursive,
    // and have only Int parameters. simp+omega works on flat match-on-Int bodies.
    if fn_names.iter().any(|n| !ctx.fn_sigs.contains_key(n)) {
        return None;
    }
    let mut wrapper_return = false;
    for item in &ctx.items {
        if let crate::ast::TopLevel::FnDef(fd) = item
            && fn_names.contains(&fd.name)
        {
            // Reject self-recursive functions: `unfold fn` only does
            // one step, so a self-recursive body leaves another call
            // to `fn` in the goal that simp can't close. Narrow check
            // — we used to reject anything in `fn_names`, but after
            // we extended `fn_names` with transitive callees the
            // narrow form (`{fd.name}`) is what we actually want;
            // calling a *different* fn that's also in the unfold
            // list is fine because the next unfold pass strips it.
            let mut self_only = std::collections::BTreeSet::new();
            self_only.insert(fd.name.clone());
            if body_calls_any_of(&fd.body, &self_only) {
                return None;
            }
            // The Int-parameter constraint only applies to the
            // top-level law function. Cross-module callees may take
            // refined types like `Natural`.
            if fd.name == vb.fn_name && fd.params.iter().any(|(_, t)| t != "Int") {
                return None;
            }
            if fd.return_type != "Int" && fd.return_type != "Float" {
                wrapper_return = true;
            }
        }
    }
    // Inspect cross-module callees via `ctx.fn_sigs` (they aren't in
    // `ctx.items`). Mark wrapper_return when their result type is a
    // wrapper (Result, Option, …).
    for name in &fn_names {
        if let Some((_params, ret, _effects)) = ctx.fn_sigs.get(name)
            && !matches!(ret, crate::types::Type::Int | crate::types::Type::Float)
        {
            wrapper_return = true;
        }
    }

    // Top-level law fn first in the unfold list — Lean needs to see
    // it in the goal before transitively-reached callees, otherwise
    // `unfold Modules.X.Y.foo` fails outright at `safeSum a b = …`.
    let mut ordered: Vec<String> = Vec::new();
    if fn_names.contains(&vb.fn_name) {
        ordered.push(vb.fn_name.clone());
    }
    for n in &fn_names {
        if n != &vb.fn_name {
            ordered.push(n.clone());
        }
    }
    let lean_names: Vec<String> = ordered.iter().map(|n| aver_name_to_lean(n)).collect();
    let simp_list = lean_names.join(", ");

    // Pull the actual guard predicate out of whatever smart
    // constructor the fn_names walk reaches. The canonical shape is
    //   fn fromX(p: Int) -> Result<NamedY, _>
    //       match <bool-expr-in-p>
    //           true  -> Result.Ok(NamedY(...))
    //           false -> Result.Err("...")
    // — i.e. a refinement-via-opaque smart constructor. We grab the
    // `<bool-expr-in-p>` subject and the param name, so when we
    // build the `by_cases` clauses below we can substitute the
    // law-quantified variable for the smart-constructor's
    // parameter and emit the *actual* guard:
    //   Nat       → `by_cases h_a : a ≥ 0`
    //   Positive  → `by_cases h_a : a > 0`
    //   Discount  → `by_cases h_a : (a ≥ 0) && (a ≤ 100)`
    // Falls back to the conservative `a ≥ 0` (the Nat shape) when
    // we can't find a matching smart constructor; lake will error
    // loudly if the pick is wrong, prompting a manual companion
    // proof rather than silently issuing `sorry`.
    let smart_guard = extract_smart_constructor_guard(&fn_names, ctx);

    if wrapper_return {
        // `simp + omega` can't close `Except.ok x = Except.ok y` —
        // omega is a linear-arithmetic decision procedure on Int,
        // blind to constructor-equality on a wrapper. The tactic
        // below was verified by hand on the sound-proof Natural
        // example (`examples/modules/natural_app.av`):
        //   1. `unfold` every user fn the law touches, top-level
        //      first so the goal exposes the call layer Lean's
        //      `unfold` operates on at each step.
        //   2. For each Int parameter, branch on `p ≥ 0` (the only
        //      predicate that fromInt-style smart constructors use
        //      to decide ok/err).
        //   3. `simp` with all introduced hypotheses + `Int.add_
        //      comm` + `Int.mul_comm` closes the resulting case
        //      grid. The arithmetic lemmas are conservative
        //      defaults — `simp` ignores them when the goal
        //      doesn't mention `+`/`*`.
        // Conservative shape: if a smart constructor uses a
        // different predicate (`x > 0`, `x ≤ 100`, …) the
        // `by_cases` pick is wrong and lake fails with a real
        // `unsolved goals` error, prompting a manual companion
        // proof. Strictly better than the silent `sorry` we used
        // to emit — `safeSum.commutative` in the sound-proof
        // Natural example actually discharges here now.
        let by_cases_clauses: Vec<String> = intro_names
            .iter()
            .map(|n| {
                let predicate = match &smart_guard {
                    Some((param, subject)) => {
                        let substituted = substitute_ident_in_expr(subject, param, n);
                        emit_expr(&substituted, ctx)
                    }
                    None => format!("{n} ≥ 0"),
                };
                format!("by_cases h_{n} : {predicate}")
            })
            .collect();
        let by_cases_chain = by_cases_clauses.join(" <;> ");
        let simp_hyps: Vec<String> = intro_names
            .iter()
            .map(|n| format!("h_{n}"))
            .chain(["Int.add_comm".to_string(), "Int.mul_comm".to_string()])
            .collect();
        let simp_args = simp_hyps.join(", ");
        Some(intro_then(
            intro_names,
            vec![
                format!("unfold {}", lean_names.join(" ")),
                format!("{by_cases_chain} <;> simp [{simp_args}]"),
            ],
        ))
    } else {
        Some(intro_then(
            intro_names,
            vec![format!("simp only [{}] <;> omega", simp_list)],
        ))
    }
}

fn body_calls_any_of(
    body: &crate::ast::FnBody,
    names: &std::collections::BTreeSet<String>,
) -> bool {
    let mut called = std::collections::BTreeSet::new();
    for stmt in body.stmts() {
        match stmt {
            crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                collect_fn_calls(e, &mut called);
            }
        }
    }
    called.iter().any(|c| names.contains(c))
}

fn collect_fn_calls(expr: &Spanned<Expr>, out: &mut std::collections::BTreeSet<String>) {
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&f.node) {
                // Skip top-level uppercase namespace handles like
                // `List.len` / `Option.Some` — those are built-in
                // namespaces, not user functions the auto-proof
                // can unfold. Cross-module user calls
                // (`Modules.X.Y.fn`) survive because the leaf
                // function name `fn` starts lower-case even when the
                // dotted prefix starts uppercase.
                let last = name.rsplit('.').next().unwrap_or(&name);
                if last.chars().next().is_some_and(|c| c.is_lowercase()) {
                    out.insert(name);
                }
            }
            for arg in args {
                collect_fn_calls(arg, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_fn_calls(l, out);
            collect_fn_calls(r, out);
        }
        Expr::Attr(obj, _) => collect_fn_calls(obj, out),
        Expr::Match { subject, arms, .. } => {
            collect_fn_calls(subject, out);
            for arm in arms {
                collect_fn_calls(&arm.body, out);
            }
        }
        Expr::TailCall(boxed) => {
            out.insert(boxed.target.clone());
            for arg in &boxed.args {
                collect_fn_calls(arg, out);
            }
        }
        _ => {}
    }
}

pub fn emit_verify_law_support_theorems(
    vb: &VerifyBlock,
    _law: &VerifyLaw,
    ctx: &CodegenContext,
    _theorem_base: &str,
) -> Vec<String> {
    collect_missing_helper_law_hints(&ctx.items, &ctx.fn_sigs)
        .into_iter()
        .find(|hint| hint.line == vb.line && hint.fn_name == vb.fn_name)
        .map(|hint| {
            vec![
                format!("-- hint: {}", missing_helper_law_message(&hint)),
                "-- hint: the main theorem can stay generic, but it still needs those helper laws as intermediate theorems".to_string(),
            ]
        })
        .unwrap_or_default()
}

pub(super) fn intro_then(intro_names: &[String], steps: Vec<String>) -> Vec<String> {
    let mut lines = Vec::new();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }
    lines.extend(steps);
    indent_lines(lines, 2)
}

fn extend_intro_names_with_premises(law: &VerifyLaw, intro_names: &[String]) -> Vec<String> {
    let mut names = intro_names.to_vec();
    if law.when.is_some() {
        names.extend(intro_names.iter().map(|name| format!("h_{name}")));
        names.push("h_when".to_string());
    }
    names
}

pub(super) fn indent_lines(lines: Vec<String>, spaces: usize) -> Vec<String> {
    let pad = " ".repeat(spaces);
    lines
        .into_iter()
        .map(|line| format!("{pad}{line}"))
        .collect()
}

/// Find a single-param smart constructor in `fn_names` whose body
/// is the canonical refinement-via-opaque shape:
///   match <subject:Bool>
///       true  -> Result.Ok(...)
///       false -> Result.Err(...)
/// Returns `(param_name, subject_expr)` of the first match. Used
/// by the wrapper-return auto-proof path so by_cases emits the
/// real guard from the source, not a hard-coded `≥ 0`.
fn extract_smart_constructor_guard(
    fn_names: &std::collections::BTreeSet<String>,
    ctx: &CodegenContext,
) -> Option<(String, Spanned<Expr>)> {
    for item in &ctx.items {
        let crate::ast::TopLevel::FnDef(fd) = item else {
            continue;
        };
        if !fn_names.contains(&fd.name) {
            continue;
        }
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if fd.params.len() != 1 {
            continue;
        }
        let (param_name, param_type) = &fd.params[0];
        if param_type != "Int" {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { subject, arms } = &body_expr.node else {
            continue;
        };
        if arms.len() != 2 {
            continue;
        }
        if !arms_are_bool_ok_err(arms) {
            continue;
        }
        return Some((param_name.clone(), (**subject).clone()));
    }
    None
}

/// True iff the two arms together look like a smart-constructor
/// branch on Bool: one arm has `Pattern::Literal(Bool(true))` and
/// produces an `Result.Ok(...)`, the other `Bool(false)` →
/// `Result.Err(...)`.
fn arms_are_bool_ok_err(arms: &[MatchArm]) -> bool {
    if arms.len() != 2 {
        return false;
    }
    let mut saw_true_ok = false;
    let mut saw_false_err = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if body_starts_with_constructor(&arm.body, "Result.Ok") {
                    saw_true_ok = true;
                }
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if body_starts_with_constructor(&arm.body, "Result.Err") {
                    saw_false_err = true;
                }
            }
            _ => return false,
        }
    }
    saw_true_ok && saw_false_err
}

/// Whether a Spanned<Expr> is a call to the named constructor
/// (`Result.Ok` / `Result.Err`). Handles both AST shapes the
/// parser can produce — `Expr::Constructor(name, ...)` and the
/// `Expr::FnCall(Expr::Attr(Expr::Ident(ns), name), ...)` form.
fn body_starts_with_constructor(expr: &Spanned<Expr>, full_name: &str) -> bool {
    match &expr.node {
        Expr::Constructor(name, _) => name == full_name,
        Expr::FnCall(callee, _) => {
            if let Expr::Attr(obj, field) = &callee.node
                && let Expr::Ident(ns) = &obj.node
            {
                let dotted = format!("{ns}.{field}");
                dotted == full_name
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Recursively substitute every `Expr::Ident(from)` with
/// `Expr::Ident(to)` inside `expr`. Used by the wrapper-return
/// auto-proof to rewrite a smart constructor's guard subject in
/// terms of the law-quantified variable: a smart constructor that
/// takes `n` and gates on `n ≥ 0` becomes `a ≥ 0` when the law
/// quantifies over `a`.
fn substitute_ident_in_expr(expr: &Spanned<Expr>, from: &str, to: &str) -> Spanned<Expr> {
    let new_node = match &expr.node {
        Expr::Ident(name) => Expr::Ident(if name == from {
            to.to_string()
        } else {
            name.clone()
        }),
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(substitute_ident_in_expr(l, from, to)),
            Box::new(substitute_ident_in_expr(r, from, to)),
        ),
        Expr::Neg(inner) => Expr::Neg(Box::new(substitute_ident_in_expr(inner, from, to))),
        Expr::Attr(inner, field) => Expr::Attr(
            Box::new(substitute_ident_in_expr(inner, from, to)),
            field.clone(),
        ),
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(substitute_ident_in_expr(callee, from, to)),
            args.iter()
                .map(|a| substitute_ident_in_expr(a, from, to))
                .collect(),
        ),
        Expr::ErrorProp(inner) => {
            Expr::ErrorProp(Box::new(substitute_ident_in_expr(inner, from, to)))
        }
        _ => expr.node.clone(),
    };
    Spanned::new(new_node, expr.line)
}

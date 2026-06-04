/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod induction;
mod sampled;
mod shared;
mod spec;

use super::VerifyEmitMode;
use super::expr::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
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
    let fn_id = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(fn_name))?;
    ctx.proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == fn_id && t.law_name == law_name)
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

    // IR-pinned `SpecEquivalence` — the lowerer validated impl and
    // spec fns have syntactically-identical bodies; backend closes
    // via `simpa [<unfolds>]` (impl + spec + transitively-reached
    // helpers). Falls through to the legacy spec dispatch when IR
    // didn't pin (other spec sub-shapes are still backend-driven).
    if let Some(crate::ir::ProofStrategy::SpecEquivalence { ref extra_unfolds }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        let lean_names: Vec<String> = extra_unfolds.iter().map(|n| aver_name_to_lean(n)).collect();
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![format!("simpa [{}]", lean_names.join(", "))],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `LinearIntSpecEquivalence` — Step 40: lowerer
    // validated substituted bodies are pure linear arithmetic over
    // Int givens. Backend emits `change <impl> = <spec> ; omega`.
    if let Some(crate::ir::ProofStrategy::LinearIntSpecEquivalence {
        ref unfolded_impl,
        ref unfolded_spec,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![
                    format!(
                        "change {} = {}",
                        super::expr::emit_expr(unfolded_impl, ctx),
                        super::expr::emit_expr(unfolded_spec, ctx)
                    ),
                    "omega".to_string(),
                ],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `SpecEquivalenceSimpNormalized` — Step 39 broaden:
    // impl and spec bodies aren't syntactically identical but
    // normalize to the same expression under arg substitution +
    // algebraic identity folding (`a + 0`, `a * 1`, `a * 0`).
    // Backend closes via `simp [<unfolds>]`; the simp normalization
    // discharges the residual arithmetic identities.
    if let Some(crate::ir::ProofStrategy::SpecEquivalenceSimpNormalized { ref extra_unfolds }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        let lean_names: Vec<String> = extra_unfolds.iter().map(|n| aver_name_to_lean(n)).collect();
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![format!("simp [{}]", lean_names.join(", "))],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `EffectfulSpecEquivalence` — Oracle Lift normalised
    // both sides; lowerer matched the canonical `impl(args) ==
    // spec(args)` shape post-rewrite. Backend emits `simp [impl,
    // spec]`; both definitions unfold to the same oracle call.
    if let Some(crate::ir::ProofStrategy::EffectfulSpecEquivalence {
        ref impl_fn,
        ref spec_fn,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![format!(
                    "simp [{}, {}]",
                    aver_name_to_lean(impl_fn),
                    aver_name_to_lean(spec_fn)
                )],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `LinearRecurrence2SpecEquivalence` — lowerer
    // validated impl as tail-rec wrapper, spec as direct second-order
    // recurrence, helper as their shared affine worker. Dispatches
    // to the existing emit which renders the Nat-helper + shift
    // lemma + helper-seed bridge (heavy ~50-line support_lines stay
    // in the legacy module). The IR pin makes the algebraic decision
    // observable in `proof_ir.law_theorems` and provides the integration
    // point for a future Dafny consumer (issue #116).
    if matches!(
        law_strategy_for(ctx, &vb.fn_name, &law.name),
        Some(crate::ir::ProofStrategy::LinearRecurrence2SpecEquivalence { .. })
    ) && let Some(proof) = spec::emit_second_order_linear_recurrence_spec_equivalence_law(
        vb,
        law,
        ctx,
        &proof_intro_names,
    ) {
        return Some(proof);
    }

    // Stage 8c of #232 — `MatchDispatcherFold`. Structural induction
    // on `xs` + per-arm `simp` + `omega` for arithmetic identity.
    if let Some(crate::ir::ProofStrategy::MatchDispatcherFold { fold_fn, spec_fn }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
        && let Some(proof) = spec::emit_match_dispatcher_fold_law(vb, law, ctx, &fold_fn, &spec_fn)
    {
        return Some(proof);
    }

    // Stage 8b of #232 — `ResultPipelineChain`. Unfold both fns +
    // every step fn, then `repeat split` peels off match layers
    // until structural equality remains.
    if let Some(crate::ir::ProofStrategy::ResultPipelineChain {
        chain_qm_fn,
        chain_manual_fn,
        step_fns,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
        && let Some(proof) = spec::emit_result_pipeline_chain_law(
            vb,
            law,
            ctx,
            &chain_qm_fn,
            &chain_manual_fn,
            &step_fns,
        )
    {
        return Some(proof);
    }

    // Stage 8 of #232 — `WrapperOverRecursion` support stack.
    // Aux acc-decomposition lemma + main universal lemma; both
    // close in core Lean 4 (`omega`) without Mathlib.
    if let Some(crate::ir::ProofStrategy::WrapperOverRecursion {
        wrapper_fn,
        inner_fn,
        other_fn,
        combine_op,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
        && let Some(proof) = spec::emit_wrapper_over_recursion_law(
            vb,
            law,
            ctx,
            &wrapper_fn,
            &inner_fn,
            &other_fn,
            combine_op,
        )
    {
        return Some(proof);
    }

    spec::emit_spec_function_equivalence_law(vb, law, ctx, &proof_intro_names)
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
                let atom_arg = |e: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>| {
                    let rendered = super::expr::emit_expr(e, ctx);
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
                let atom_render = |e: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>| {
                    let rendered = super::expr::emit_expr(e, ctx);
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
                let atom_render = |e: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>| {
                    let rendered = super::expr::emit_expr(e, ctx);
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
                        law.when.is_some(),
                        ctx,
                    ),
                    replaces_theorem: false,
                });
            }
            None
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
    has_when: bool,
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
                        let substituted = crate::codegen::common::substitute_ident_in_resolved_expr(
                            &g.predicate,
                            &g.param,
                            n,
                        );
                        super::expr::emit_expr(&substituted, ctx)
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
        // A `when` premise is introduced as a hypothesis (`h_when`).
        // `simp_all` simplifies it — e.g. a nested Bool `match` lowered to
        // an `if/then/else` — so `omega` can use the premise; for an
        // unsatisfiable premise it derives the contradiction and closes
        // the (vacuously-true) law. Plain `simp only` leaves the Bool
        // premise opaque and `omega` fails ("no usable constraints"),
        // wrongly rejecting a valid law. Without a `when`, keep the
        // conservative `simp only` — there is no premise to simplify.
        let tac = if has_when {
            format!("simp_all [{}] <;> omega", lean_names.join(", "))
        } else {
            format!("simp only [{}] <;> omega", lean_names.join(", "))
        };
        intro_then(intro_names, vec![tac])
    }
}

pub fn emit_verify_law_support_theorems(
    vb: &VerifyBlock,
    _law: &VerifyLaw,
    ctx: &CodegenContext,
    _theorem_base: &str,
) -> Vec<String> {
    collect_missing_helper_law_hints(&ctx.items, ctx)
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

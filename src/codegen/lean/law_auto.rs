/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod decimal;
mod floor_window;
mod induction;
mod sampled;
mod shared;
mod spec;
mod suffix_roundtrip;

use super::VerifyEmitMode;
use super::expr::aver_name_to_lean;
use super::recursive_pure_fn_names;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::{collect_missing_helper_law_hints, missing_helper_law_message};
use sampled::emit_guarded_domain_law;

/// Cross-file law pool — EMIT-side gate (re-exported for `transpile`):
/// the `(module_prefix, theorem_base)` dep-law theorems some consumer law
/// admits, so the emitter writes ONLY those into the build.
pub(crate) use induction::admitted_dep_law_theorems;

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
    // Scope-aware: a dep module's law is keyed by its dep-scope `FnId`
    // (cross-file law pool), so resolve `fn_name` through the active
    // module scope rather than assuming entry.
    let fn_id = ctx.law_target_fn_id(fn_name)?;
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
    let inner = emit_verify_law_forall_auto_proof_inner(
        vb,
        law,
        ctx,
        verify_mode,
        theorem_base,
        quant_params,
        theorem_prop,
    )?;
    // ADDITIVE, SHAPE-GATED `grind` outright-closer rung — prepended at
    // the TOP of the ladder, but ONLY for laws whose goal is
    // grind-amenable (algebraic / ring / order / comparison identity, NOT
    // a structural-induction theorem). `grind` (Lean 4.31 core, no
    // Mathlib) is a saturation solver with commutative-ring (+ring), AC,
    // and order/linarith subsolvers — it closes flat arithmetic goals
    // (like the nonlinear NR polynomial identity in nr_wall) WITHOUT
    // induction, but it CANNOT perform the structural induction the
    // recursive monoid/order laws (append-assoc, rev-involution,
    // count-rev) require. Emitting it on those inductive goals only burns
    // ~1.2s per proof for a guaranteed fall-through. The rung is admitted
    // iff BOTH gates pass: the SHAPE gate (`grind_is_shape_amenable` — the
    // law's unfold cone has NO user-recursive pure fn the proof would
    // induct over) AND the FRONTIER gate (the proof ends in an honest
    // `sorry`, so grind can do NEW work — guaranteed closers like
    // `omega`/`rfl` are left byte-identical).
    Some(maybe_wrap_with_grind_rung(vb, law, ctx, inner))
}

/// Whether the additive `grind` rung is SHAPE-AMENABLE for this law:
/// the goal is a flat algebraic / ring / order / comparison identity
/// that `grind`'s quantifier-free ring/AC/order subsolvers can close
/// outright, NOT a structural-induction theorem over a user-recursive
/// def (which needs `induction`/`fun_induction` first — something
/// `grind` does not perform).
///
/// The discriminating signal is the law's unfold cone (`law_simp_defs`,
/// the same transitively-reached pure-fn set the simp rungs source)
/// intersected with `recursive_pure_fn_names` (the recursion classifier's
/// set of user-recursive pure fns). When the cone touches ANY recursive
/// pure fn, the law routes to the structural-induction path
/// (`mk_arms`/list-induction/`fun_induction`) and `grind` cannot close
/// it — SKIP. When the cone is entirely non-recursive, the goal is flat
/// arithmetic/ring (like `nrNewErrNum = nrOldErrSq`) — ADMIT.
///
/// Also requires a non-empty cone: a law with no user fns in its cone is
/// a pure builtin/arith goal the existing IR-pinned rungs (omega /
/// RingIdentity) already own; a bare `grind` adds nothing there.
fn grind_is_shape_amenable(ctx: &CodegenContext, vb: &VerifyBlock, law: &VerifyLaw) -> bool {
    // Cone by SOURCE name (pre-`aver_name_to_lean`), so it can be matched
    // directly against `recursive_pure_fn_names`' bare source names.
    let cone = shared::law_simp_source_names(ctx, vb, law);
    if cone.is_empty() {
        return false;
    }
    let recursive = recursive_pure_fn_names(ctx);
    // A user-recursive pure fn anywhere in the cone => the law inducts;
    // `grind` cannot close it. Skip the rung (no waste on inductive goals).
    !cone.iter().any(|name| recursive.contains(name))
}

/// Wrap an already-emitted [`AutoProof`] with the additive top-of-ladder
/// `grind` rung when the law is shape-amenable (see
/// [`grind_is_shape_amenable`]) AND the proof body has the canonical
/// `intro <givens>; <body>` shape. Returns the proof unchanged otherwise
/// — the rung is purely additive (a non-closing `grind` ends in `done`
/// and `first` falls through to the existing body byte-for-byte).
fn maybe_wrap_with_grind_rung(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    proof: AutoProof,
) -> AutoProof {
    // `replaces_theorem` proofs carry their OWN theorem statement +
    // bespoke support stack (RingIdentity, floor-window, decimal /
    // string roundtrips) — there is no plain `intro <givens>; <tactic>`
    // body to prepend a `grind` arm onto, and the RingIdentity family
    // already runs `grind` internally. Leave them untouched.
    if proof.replaces_theorem {
        return proof;
    }
    // A `when`-premised law introduces extra hypotheses (`h_when`, …)
    // whose Subtype/`by_cases` handling the existing rungs are written
    // around; a bare `grind` over the premise shape risks an
    // introduced-but-unused hypothesis stall. The plain unconditional
    // laws are exactly grind's wheelhouse, so restrict the rung to them.
    if law.when.is_some() {
        return proof;
    }
    // A proof body that ALREADY leads with `grind` owns its own grind
    // rung (the `RingIdentity` family emits `first | (grind [<cone>]; …`
    // internally). Re-wrapping it would emit a redundant outer `grind`
    // arm — pure waste (a second grind saturation on the same goal). Skip.
    if proof.proof_lines.iter().any(|l| l.contains("grind")) {
        return proof;
    }
    // SHAPE GATE: only wrap a grind-amenable (flat algebraic/ring/order)
    // goal — never an inductive law over a user-recursive def.
    if !grind_is_shape_amenable(ctx, vb, law) {
        return proof;
    }
    // FRONTIER GATE: only wrap a proof whose body ends in an honest
    // `sorry` floor (a `first | … | sorry` fallback that might NOT
    // close). A proof with a GUARANTEED closer (no trailing `sorry`: the
    // IR-pinned `Reflexive`/`Commutative`/`LinearArithmetic`/… rungs that
    // end in `rfl`/`omega`/`simp …`) already closes cheaply and reliably;
    // prepending `grind` there only (a) churns the byte-for-byte golden
    // exports for zero benefit and (b) risks running `grind` to a
    // potential `maxHeartbeats` build error `first` cannot catch on a goal
    // `omega` decides instantly. The shape gate already restricts to flat
    // goals; pairing it with the frontier gate means `grind` is emitted
    // ONLY where it can do NEW work — exactly the open flat laws (like the
    // nonlinear ring identity the simp package no longer normalizes).
    let ends_in_sorry = proof
        .proof_lines
        .iter()
        .rev()
        .find(|l| !l.trim().is_empty())
        .is_some_and(|l| l.trim_end().ends_with("sorry") || l.trim_end().ends_with("sorry)"));
    if !ends_in_sorry {
        return proof;
    }
    // The proof must be the canonical `intro <givens>` + body shape
    // (the first proof line, post-indent, is the `intro`). Anything
    // else is left as-is rather than guessing where to splice the arm.
    let Some(first_line) = proof.proof_lines.first() else {
        return proof;
    };
    if !first_line.trim_start().starts_with("intro ") {
        return proof;
    }
    // The unfold cone (Lean names, law subject first) the same way the
    // simp rungs source it. `grind` does NOT unfold user `def`s on its
    // own (the same lesson as the RingIdentity rung), so it is handed the
    // cone as `grind [<cone>]`. The shape gate already guaranteed a
    // non-empty, recursion-free cone.
    let cone: Vec<String> = shared::law_simp_defs(ctx, vb, law).into_iter().collect();
    if cone.is_empty() {
        return proof;
    }
    let mut proof_lines = proof.proof_lines;
    let intro_line = proof_lines.remove(0);
    let wrapped = wrap_body_with_grind_rung(intro_line, proof_lines, &cone);
    AutoProof {
        support_lines: proof.support_lines,
        proof_lines: wrapped,
        replaces_theorem: false,
    }
}

/// Prepend the `grind` outright-closer arm to an existing (post-`intro`)
/// `body_lines` tactic block:
///
/// ```text
/// intro <givens>
/// first
/// | (grind [<cone>]; done)
/// | (<existing body, re-indented>)
/// ```
///
/// The `grind` arm ends in `done`, so a `grind` that leaves a residual
/// goal THROWS and `first` falls to the existing body byte-for-byte
/// (modulo a 2-space indent under the final `first` arm). `grind` is a
/// sound saturation solver (it closes only true goals; on failure it
/// throws on the leftover), so the rung is purely additive — it can only
/// ADD closures and adds no axioms beyond grind's whitelisted set
/// ({propext, Classical.choice, Quot.sound}).
fn wrap_body_with_grind_rung(
    intro_line: String,
    body_lines: Vec<String>,
    cone: &[String],
) -> Vec<String> {
    let mut out = vec![intro_line, "  first".to_string()];
    out.push(format!("  | (grind [{}]; done)", cone.join(", ")));
    out.push("  | (".to_string());
    for line in &body_lines {
        out.push(format!("  {line}"));
    }
    if let Some(last) = out.last_mut() {
        last.push(')');
    }
    out
}

fn emit_verify_law_forall_auto_proof_inner(
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
    //
    // `SimpOverLemmas(names)` (the discovery feedback loop — the CLI
    // re-pins an `Induction` law when committed discovered lemmas are
    // in-scope) routes to the SAME induction emit with the lemma
    // names threaded through: the emit embeds the lemma texts, adds
    // the names to the simp sets, and tries a lemma-first fast path
    // before inducting.
    let pinned = law_strategy_for(ctx, &vb.fn_name, &law.name);
    let discovered: Vec<String> = match &pinned {
        Some(crate::ir::ProofStrategy::SimpOverLemmas(names)) => names.clone(),
        _ => Vec::new(),
    };
    if matches!(
        pinned,
        Some(crate::ir::ProofStrategy::Induction { .. })
            | Some(crate::ir::ProofStrategy::SimpOverLemmas(_))
    ) && let Some(proof) = induction::emit_structural_induction_law(
        vb,
        law,
        ctx,
        &intro_names,
        theorem_base,
        quant_params,
        theorem_prop,
        &discovered,
    ) {
        return Some(proof);
    }
    // IR-pinned `FloorDivWindow` — the floor-division window family.
    // The renderer emits a self-contained support stack (power
    // algebra by functional induction, the binary-exponent window
    // characterization, the core ediv bridges) plus the law theorem
    // in TRUE universal form, so it replaces the bounded statement
    // entirely (`replaces_theorem`). The caller's law-class marker
    // recognizes the pin and classes the statement `universal`;
    // crediting stays fail-closed behind the `#print axioms`
    // whitelist.
    if let Some(proof) =
        floor_window::emit_floor_window_law(vb, law, ctx, theorem_base, quant_params)
    {
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

    // IR-pinned `EnumConstantFold` — the lowerer validated the law
    // pins every non-Int param to a constructor literal and the call
    // tree is non-recursive, so the goal is a ground equality. Unfold
    // the fn + transitively-reached callees with `simp only`, then
    // close each residual branch with a `split`/`rfl`/`decide` cascade.
    // The `first | ... ` tries the cheapest closer first; the kernel
    // re-checks whichever fires, and a goal the cascade can't close
    // surfaces honestly (the law would then need a hand proof) rather
    // than a false `sorry`-free claim.
    if let Some(crate::ir::ProofStrategy::EnumConstantFold { ref unfold_fns }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        let lean_names: Vec<String> = unfold_fns.iter().map(|n| aver_name_to_lean(n)).collect();
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![format!(
                    "simp only [{}] <;> (first | (split <;> rfl) | rfl | decide)",
                    lean_names.join(", ")
                )],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `FiniteDomainCases` — every given ranges over a closed
    // finite domain (Bool or an all-fieldless user enum, ≤ 16 total
    // combinations), so exhaustive `cases` enumeration yields one
    // closed ground goal per combination. Fuel-wrapped callees compute
    // through (constant-measure constructor args), which is why the
    // detector has no call-shape / recursion gate. Per-leaf cascade:
    // `rfl` first (the workhorse — kernel defeq computes ground
    // terms), then `decide` (kernel-rechecked; can be weak on derived
    // DecidableEq over nested ADTs), then the iron guard — an HONEST
    // `sorry`. A non-closing leaf must surface as a caught sorry,
    // NEVER a build error and NEVER `native_decide` (which would
    // inject `Lean.ofReduceBool` and silently kill `universal:true`).
    if let Some(crate::ir::ProofStrategy::FiniteDomainCases { ref givens }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
    {
        let cascade = givens
            .iter()
            .map(|g| format!("cases {}", aver_name_to_lean(g)))
            .collect::<Vec<_>>()
            .join(" <;> ");
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                &proof_intro_names,
                vec![format!("{cascade} <;> (first | rfl | decide | sorry)")],
            ),
            replaces_theorem: false,
        });
    }

    // IR-pinned `IntDecimalRoundtrip` — the lowerer validated the
    // ENTIRE canonical decimal-parser shape (head-char dispatch arms,
    // single recognized string-pos scanner with the synthesized
    // `__fuel_scan` companion, slice + `Int.fromString` leaf), so the
    // backend renders the fixed sign-split skeleton ported from the
    // verified json hand proof. Wrapped in `first | (…; done) | sorry`
    // — a non-closing case degrades to a caught honest sorry.
    if let Some(crate::ir::ProofStrategy::IntDecimalRoundtrip {
        ref parse_fn,
        ref neg_fn,
        ref pos_fn,
        ref sign_fn,
        ref scanner_fn,
        ref predicate_fn,
        ref finish_fn,
        ref finish_int_fn,
        ref serializer_fn,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
        && let Some(proof) = decimal::emit_int_decimal_roundtrip_law(
            law,
            ctx,
            theorem_base,
            parse_fn,
            neg_fn,
            pos_fn,
            sign_fn,
            scanner_fn,
            predicate_fn,
            finish_fn,
            finish_int_fn,
            serializer_fn,
        )
    {
        return Some(proof);
    }

    // IR-pinned `StringEscapeRoundtrip` — the lowerer validated the
    // ENTIRE escaped-string roundtrip pair (scanner SCC arm shapes,
    // producer classifier table aligned with the consumer's escape
    // dispatcher, control-escape prefix, threshold agreement), so the
    // backend renders the suffix-invariant proof skeleton ported from
    // the verified json hand proof. Every synthesized lemma and the
    // law proof itself carry `first | (…; done) | sorry` floors — a
    // non-closing template degrades to caught honest sorries, never a
    // build error.
    if let Some(crate::ir::ProofStrategy::StringEscapeRoundtrip(ref pin)) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
        && let Some(proof) =
            suffix_roundtrip::emit_string_escape_roundtrip_law(law, ctx, theorem_base, pin)
    {
        return Some(proof);
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
                // `when`-premise + wrapper-return: DECLINE. The
                // sign-split `by_cases <;> simp` chain this arm
                // renders cannot consume a `when` premise (the
                // hypothesis is introduced but never used), and the
                // pre-fix emission even cased over the introduced
                // HYPOTHESIS names (`by_cases h_h_a : h_a >= 0` where
                // `h_a : Prop`) — an application-type-mismatch BUILD
                // ERROR, not a caught sorry. Falling through reaches
                // `emit_guarded_domain_law`, which closes the bounded
                // guarded statement by domain case-split +
                // `native_decide`.
                if wrapper_return && !lifted && law.when.is_some() {
                    return None;
                }
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
                let uses_max_min = linear_arith_uses_max_min(unfold_fns, ctx);
                return Some(AutoProof {
                    support_lines: Vec::new(),
                    proof_lines: emit_simp_omega_from_ir(
                        unfold_fns,
                        wrapper_return,
                        smart_guard.as_ref(),
                        lifted,
                        chosen_intro,
                        &intro_names,
                        law.when.is_some(),
                        uses_max_min,
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
        .or_else(|| {
            // IR-pinned `RingIdentity` — rendered (like the prelude
            // rung below) after every legacy ad-hoc fallback, so it
            // fires only where the chain used to fall through. The
            // emit carries the strategy-scoped AC-ring package; see
            // `emit_ring_identity_law`.
            emit_ring_identity_law(vb, law, ctx, &proof_intro_names)
        })
        .or_else(|| {
            // IR-pinned `SimpOverPreludeLemmas` — DELIBERATELY the last
            // rung, after every legacy ad-hoc fallback above: it must
            // fire only where the chain used to return `None` and the
            // caller emitted a bare-`sorry` universal. The simp set is
            // kept minimal (cone + fuel + registry hits + the single
            // baked-in `Int.add_sub_cancel` rewrite the archived
            // hand-proofs needed) — a fat set risks a simp LOOP, and a
            // maxHeartbeats blow-up is a build error `first` cannot
            // catch. `done` forces closure: simp succeeding but
            // leaving a residual goal must fall to the honest `sorry`,
            // never surface as an "unsolved goals" build error.
            emit_simp_over_prelude_lemmas_law(vb, law, ctx, &proof_intro_names)
        })
}

/// Fixed core AC-ring lemma package for the `RingIdentity` rung —
/// `Init.Data.Int.Lemmas` names only (core + Std, NO Mathlib, no
/// `ring` tactic). Mechanism: `Int.mul_add`/`Int.add_mul` fully
/// distribute products over sums, the mul/add `comm`/`left_comm`/
/// `assoc` triples AC-sort monomials and sums (simp's ordered
/// rewriting terminates on permutational lemmas), and
/// `Int.sub_eq_add_neg`/`Int.zero_sub`/`Int.neg_mul` push `-` into
/// the same normal form. An unconditional ring identity then has
/// identical monomial multisets on both sides — no coefficient
/// collection needed — and the default simp set (`beq_iff_eq`,
/// `Int.mul_one`, `Int.add_zero`, …) finishes.
///
/// SCOPED TO THIS RUNG ONLY: the permutational rewrites loop or
/// destroy the normal forms other strategies' simp sets rely on, so
/// they are never merged into the shared prelude registry
/// (`prelude_spec_lemmas_for_builtins`) or any other strategy's set.
///
/// Known boundary (measured): identities that need coefficient
/// collection (`t + t` vs `2 * t`) or cancellation
/// (`x + (-x) = 0` nested inside products) stay outside — the
/// `first | … | sorry` alternation degrades them to an honest
/// caught sorry.
const RING_NORMALIZATION_LEMMAS: [&str; 11] = [
    "Int.mul_add",
    "Int.add_mul",
    "Int.mul_comm",
    "Int.mul_left_comm",
    "Int.mul_assoc",
    "Int.add_comm",
    "Int.add_left_comm",
    "Int.add_assoc",
    "Int.sub_eq_add_neg",
    "Int.zero_sub",
    "Int.neg_mul",
];

/// Render the `RingIdentity` rung:
/// `intro <givens>; first | (simp [<cone>, <AC-ring package>]; done) | sorry`.
///
/// Simp-set assembly: the unfold cone (law subject first — source
/// names via `aver_name_to_lean`; record-field projections unfold by
/// themselves), then the fixed [`RING_NORMALIZATION_LEMMAS`] package.
/// `done` forces closure — a simp that fails OR leaves a residual
/// goal falls to the honest caught `sorry`, never an "unsolved
/// goals" build error and never `native_decide`.
fn emit_ring_identity_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    proof_intro_names: &[String],
) -> Option<AutoProof> {
    let Some(crate::ir::ProofStrategy::RingIdentity { unfold_fns }) =
        law_strategy_for(ctx, &vb.fn_name, &law.name)
    else {
        return None;
    };
    // Cone unfold fns, mapped to their Lean names. `grind` does NOT
    // unfold user `def`s on its own, so the same cone the simp set
    // leads with is handed to `grind [<cone>]` as its unfold bracket;
    // without it `grind` stalls on the opaque `sameValue …` goal.
    let grind_cone: Vec<String> = unfold_fns.iter().map(|f| aver_name_to_lean(f)).collect();
    let simp_set: Vec<String> = grind_cone
        .iter()
        .cloned()
        .chain(RING_NORMALIZATION_LEMMAS.iter().map(|s| s.to_string()))
        .collect();
    Some(AutoProof {
        support_lines: Vec::new(),
        // Rung order, cheapest genuine closer first: `grind [<cone>]`
        // (Lean 4.31 core, no Mathlib) carries the multi-variable
        // nonlinear ring identities the hand-rolled AC-ring simp
        // package stopped normalizing at 4.31 (`minus.equalsPlusNegate`);
        // the simp package stays as the fallback rung, then the honest
        // caught `sorry`. Every alternation arm ends in `done`, so a
        // tactic that leaves a residual goal falls through rather than
        // raising an "unsolved goals" build error.
        proof_lines: intro_then(
            proof_intro_names,
            vec![format!(
                "first | (grind [{}]; done) | (simp [{}]; done) | sorry",
                grind_cone.join(", "),
                simp_set.join(", ")
            )],
        ),
        replaces_theorem: false,
    })
}

/// Render the `SimpOverPreludeLemmas` rung:
/// `intro <givens>; first | (simp [<set>]; done) | sorry`.
///
/// Simp-set assembly, in order: unfold fns (subject first — source
/// names via `aver_name_to_lean`), then per fuel fn the wrapper name +
/// `<fn>__fuel` + measure helpers (probed from the actual proof-mode
/// emission by `toplevel::law_fuel_simp_names` — a fuel fn that
/// graduated to native `termination_by` contributes its wrapper name
/// only, never a non-existent `__fuel` constant), then the prelude
/// spec lemmas the registry maps from the cone's builtin calls
/// (`lean::prelude_spec_lemmas_for_builtins` — the same names whose
/// mention makes the demand-driven prelude ship the lemma texts), then
/// `Int.add_sub_cancel`.
fn emit_simp_over_prelude_lemmas_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    proof_intro_names: &[String],
) -> Option<AutoProof> {
    let Some(crate::ir::ProofStrategy::SimpOverPreludeLemmas {
        unfold_fns,
        fuel_fns,
        builtins,
    }) = law_strategy_for(ctx, &vb.fn_name, &law.name)
    else {
        return None;
    };
    let mut simp_set: Vec<String> = Vec::new();
    let push_unique = |set: &mut Vec<String>, name: String| {
        if !set.contains(&name) {
            set.push(name);
        }
    };
    for f in &unfold_fns {
        push_unique(&mut simp_set, aver_name_to_lean(f));
    }
    for f in &fuel_fns {
        push_unique(&mut simp_set, aver_name_to_lean(f));
        for name in super::toplevel::law_fuel_simp_names(f, ctx) {
            push_unique(&mut simp_set, name);
        }
    }
    for lemma in super::prelude_spec_lemmas_for_builtins(&builtins) {
        push_unique(&mut simp_set, lemma);
    }
    push_unique(&mut simp_set, "Int.add_sub_cancel".to_string());
    Some(AutoProof {
        support_lines: Vec::new(),
        proof_lines: intro_then(
            proof_intro_names,
            vec![format!(
                "first | (simp [{}]; done) | sorry",
                simp_set.join(", ")
            )],
        ),
        replaces_theorem: false,
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
#[allow(clippy::too_many_arguments)]
fn emit_simp_omega_from_ir(
    unfold_fns: &[String],
    wrapper_return: bool,
    smart_guard: Option<&crate::ir::SmartGuard>,
    lifted: bool,
    intro_names: &[String],
    given_names: &[String],
    has_when: bool,
    uses_max_min: bool,
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
        // Case ONLY over the law's actual Int-typed given variables —
        // NEVER the introduced premise-hypothesis names. `intro_names`
        // carries `h_a` / `h_when` for `when`-laws, and casing over
        // those (`by_cases h_h_a : h_a >= 0` where `h_a : Prop`) is an
        // application-type-mismatch build error. (The `when`+wrapper
        // combination is declined before this arm is reached; the
        // given-name restriction here is the structural guarantee.)
        let by_cases_clauses: Vec<String> = given_names
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
        let simp_hyps: Vec<String> = given_names
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
        let tac = if uses_max_min {
            // `Int.max`/`Int.min` lower to core `max`/`min`, which
            // `omega` treats as opaque atoms (it has no `max`/`min`
            // theory). Unfolding `Int.max_def`/`Int.min_def` exposes
            // the underlying `if a ≤ b then …` so `split` peels each
            // branch into a linear goal `omega` can close. `try split`
            // (not bare `split`) keeps this safe when the unfold leaves
            // nothing to split — a bare `split` on a split-free goal is
            // an error. `simp_all` discharges the residual `if`/premise
            // shape before `omega`. GATED: only laws whose unfold chain
            // actually calls `Int.max`/`Int.min` take this path; every
            // other linear-arithmetic law stays byte-identical to the
            // `simp only [...] <;> omega` / `simp_all [...] <;> omega`
            // forms below so no existing law regresses.
            let lemmas: Vec<String> = lean_names
                .iter()
                .cloned()
                .chain(["Int.max_def".to_string(), "Int.min_def".to_string()])
                .collect();
            format!(
                "simp only [{}] <;> (try split) <;> simp_all <;> omega",
                lemmas.join(", ")
            )
        } else if has_when {
            format!("simp_all [{}] <;> omega", lean_names.join(", "))
        } else {
            format!("simp only [{}] <;> omega", lean_names.join(", "))
        };
        intro_then(intro_names, vec![tac])
    }
}

/// Detect whether the LinearArithmetic unfold chain calls
/// `Int.max` / `Int.min`. Drives the gated `Int.max_def`/`Int.min_def`
/// split tactic in [`emit_simp_omega_from_ir`] — only laws that
/// actually involve min/max take the split path; everything else stays
/// on the byte-identical `simp only`/`simp_all` + `omega` forms.
///
/// Walks the *bodies* of every fn named in `unfold_fns` (the same fns
/// the tactic will `simp only [...]`-unfold), so an `Int.max` buried in
/// a helper that the law transitively unfolds is still caught. Resolves
/// each name via the symbol table (entry scope, then every dep module),
/// matching how the unfold list itself is sourced.
fn linear_arith_uses_max_min(unfold_fns: &[String], ctx: &CodegenContext) -> bool {
    unfold_fns.iter().any(|name| {
        let fd = ctx.fn_def_by_name(name, None).or_else(|| {
            ctx.modules
                .iter()
                .find_map(|m| ctx.fn_def_by_name(name, Some(&m.prefix)))
        });
        fd.is_some_and(|fd| fn_body_calls_max_min(&fd.body))
    })
}

/// True when `body` contains a call to `Int.max` or `Int.min`.
fn fn_body_calls_max_min(body: &crate::ast::FnBody) -> bool {
    body.stmts().iter().any(|stmt| match stmt {
        crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => expr_calls_max_min(e),
    })
}

fn expr_calls_max_min(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(f, args) => {
            let is_max_min = crate::codegen::common::expr_to_dotted_name(&f.node)
                .is_some_and(|n| n == "Int.max" || n == "Int.min");
            is_max_min || args.iter().any(expr_calls_max_min)
        }
        Expr::BinOp(_, l, r) => expr_calls_max_min(l) || expr_calls_max_min(r),
        Expr::Neg(inner) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            expr_calls_max_min(inner)
        }
        Expr::Match { subject, arms } => {
            expr_calls_max_min(subject) || arms.iter().any(|arm| expr_calls_max_min(&arm.body))
        }
        Expr::Constructor(_, Some(inner)) => expr_calls_max_min(inner),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_calls_max_min)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_calls_max_min(k) || expr_calls_max_min(v)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_calls_max_min(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_calls_max_min(base) || updates.iter().any(|(_, e)| expr_calls_max_min(e))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(expr_calls_max_min),
        _ => false,
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

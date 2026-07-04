use super::VerifyEmitMode;
use super::expr::{aver_name_to_lean, emit_expr_legacy};
use super::law_auto::{emit_verify_law_forall_auto_proof, emit_verify_law_support_theorems};
use super::types::type_annotation_to_lean;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

const BOUNDED_LAW_DOMAIN_VALUE_EDGE: usize = 128;
const BOUNDED_LAW_DOMAIN_CASE_EDGE: usize = 512;
const LAW_SAMPLE_CAP: usize = 512;

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

/// Render a per-sample instantiated `when` guard with every Int literal
/// ascribed (`(4 : Int)`).
///
/// The guard is the parser's SUBSTITUTED premise: numeral literals stand
/// where Int givens stood. A bare Lean numeral in a comparison elaborates
/// as `Nat`, and with subtraction in the premise truncated `Nat`
/// subtraction changes the proposition — e.g. the probe's
/// `((((1 * 1) - 4) * ((1 * 1) - 4)) <= 4)` is TRUE over `Nat`
/// (`1 - 4 = 0`) but FALSE over `Int` (`(-3) * (-3) = 9`), so the emitted
/// `_sample_N` / `_checked_domain` theorem was FALSE AS STATED and
/// `native_decide` failed the build on a law the VM verifies. Ascribing
/// pins every literal to `Int` — the type the substituted given had.
///
/// Recurses through the operator shapes a premise is built from
/// (comparisons, arithmetic, `&&`-conjunction of multiple `when`s,
/// negation); anything else (fn calls, idents, record literals) falls
/// back to `emit_expr`, where Lean already types the positions from
/// signatures.
pub(super) fn emit_sample_guard(guard: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    let active = ctx.active_module_scope();
    let resolved = ctx.resolve_expr(guard, active.as_deref());
    emit_sample_guard_resolved(&resolved, ctx)
}

fn emit_sample_guard_resolved(
    expr: &Spanned<crate::ir::hir::ResolvedExpr>,
    ctx: &CodegenContext,
) -> String {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match &expr.node {
        ResolvedExpr::Literal(Literal::Int(i)) => format!("({} : Int)", i),
        ResolvedExpr::Neg(inner) => format!("(-{})", emit_sample_guard_resolved(inner, ctx)),
        // Multiple `when` clauses parse into a `Bool.and` chain (and a
        // negated premise into `Bool.not`) — recurse through the Bool
        // combinators with the exact spellings `lean::builtins` uses so
        // literals INSIDE the conjunction stay ascribed.
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.and")
                && args.len() == 2 =>
        {
            format!(
                "({} && {})",
                emit_sample_guard_resolved(&args[0], ctx),
                emit_sample_guard_resolved(&args[1], ctx)
            )
        }
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.or")
                && args.len() == 2 =>
        {
            format!(
                "({} || {})",
                emit_sample_guard_resolved(&args[0], ctx),
                emit_sample_guard_resolved(&args[1], ctx)
            )
        }
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.not")
                && args.len() == 1 =>
        {
            format!("(!{})", emit_sample_guard_resolved(&args[0], ctx))
        }
        ResolvedExpr::BinOp(op, left, right) => {
            let l = emit_sample_guard_resolved(left, ctx);
            let r = emit_sample_guard_resolved(right, ctx);
            // Operator spellings mirror `expr::emit_expr` exactly.
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            format!("({} {} {})", l, op_str, r)
        }
        _ => super::expr::emit_expr(expr, ctx),
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
        let left_str = emit_expr_legacy(left, ctx, None);
        // Expected side: prefer the VM ground-truth literal over the source
        // RHS. A source RHS that calls a user fn (`verify f: f(x) => g(x)`)
        // routes BOTH sides through the model — vacuously true under fuel
        // exhaustion (panic returns `default` for both). The literal pins
        // the equation to the value the program actually computed. Cases
        // without an entry (verify failed/skipped, Float-carrying value —
        // decimal repr isn't bit-exact — or a shape that doesn't round-trip)
        // keep the source RHS and rely on the `--check` panic gate.
        let right_str = super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
            .unwrap_or_else(|| emit_expr_legacy(right, ctx, None));
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
            let lhs_summary = emit_expr_legacy(left, ctx, None);
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
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode.clone(),
        );
        let rhs_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            &synthetic_law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode,
        );

        let lhs_str = emit_expr_legacy(&lhs_rw, ctx, None);
        let rhs_str = emit_expr_legacy(&rhs_rw, ctx, None);

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
        let header = match canonical_spec_ref(&vb.fn_name, law, ctx) {
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
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, ctx);
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
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let law_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
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
    let lhs_template = emit_expr_legacy(&law_lhs, ctx, None);
    let rhs_template = emit_expr_legacy(&law_rhs, ctx, None);
    // The `when` clause references the same oracle bindings the law
    // body does, so it needs the same subtype projection. Without this
    // a `when rng(root, 0, 1, 6) >= 1` clause would emit `rng ...`
    // against a `RandomIntInBounds` parameter and Lean would reject
    // the type mismatch.
    let when_template = law.when.as_ref().map(|expr| {
        let oracle_projected = crate::codegen::common::rewrite_effectful_calls_in_law(
            expr,
            law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
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
        emit_expr_legacy(&val_projected, ctx, None)
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
        // Flatten to one physical line: a `--` line comment only covers
        // the first line, so a multi-line premise (e.g. a nested Bool
        // match lowering to a multi-line `if/then/else`) would leak its
        // continuation lines out as stray Lean commands ("unexpected
        // token 'else'").
        let when_comment = emit_expr_legacy(when_expr, ctx, None).replace('\n', " ");
        lines.push(format!("-- when {when_comment}"));
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
    let pinned_law_strategy = ctx
        .law_target_fn_id(&vb.fn_name)
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| &t.strategy);
    let ir_strategy_closes_const_rhs = pinned_law_strategy.is_some_and(|s| {
        !matches!(
            s,
            crate::ir::ProofStrategy::Induction { .. }
                | crate::ir::ProofStrategy::SimpOverLemmas(_)
                | crate::ir::ProofStrategy::BackendDispatch
                | crate::ir::ProofStrategy::Sorry
                // SimpOverPreludeLemmas is a best-effort rung with an
                // honest `sorry` floor — it does NOT promise to close
                // a constant-RHS shape, so a singleton-given +
                // constant-RHS law keeps today's skip (sample +
                // checked_domain cover the point) instead of gaining
                // a universal that would land as a caught sorry.
                | crate::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
                // RingIdentity shares the prelude rung's honest-sorry
                // floor (`first | (…; done) | sorry`) — it does NOT
                // promise to close, so a singleton-given +
                // constant-RHS law keeps today's skip.
                | crate::ir::ProofStrategy::RingIdentity { .. }
                // NonlinearNonneg (the inequality sibling) has the same
                // honest-sorry floor (`first | (…) | sorry`), and its
                // detector requires a given-dependent `E >= 0` claim, so
                // the singleton-const-rhs skip can't apply anyway —
                // listed for the same conservatism as RingIdentity.
                | crate::ir::ProofStrategy::NonlinearNonneg { .. }
                // IntDecimalRoundtrip shares the same honest-sorry
                // floor (`first | (…; done) | sorry`); its detector
                // also requires a given-dependent rhs, so the
                // singleton-const-rhs skip can't apply — listed for
                // the same conservatism.
                | crate::ir::ProofStrategy::IntDecimalRoundtrip { .. }
                // StringEscapeRoundtrip: same honest-sorry floor and
                // same given-dependent-rhs detector gate — listed for
                // the same conservatism.
                | crate::ir::ProofStrategy::StringEscapeRoundtrip(_)
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
    //
    // EXCEPTION — `FiniteDomainCases`: closed enumeration defeats
    // fuel. The strategy's `cases` cascade reduces the universal to
    // ground goals over constant-measure constructor args, which
    // compute straight through `<fn>__fuel` wrappers (`rfl`/`decide`
    // evaluate them like the per-sample lemmas do). Skipping here
    // would drop the very theorems the strategy exists to close
    // (e.g. `parseEscape.escapeCodeRoundtrip`, whose `parseEscape`
    // is fuel-bounded), so the fuel gate does not apply.
    // EXCEPTION — `TailRecFixedBaseFold`: the `qexp` loop is rejected by the
    // recursion classifier (a growing accumulator), so it lands in
    // `unclassified_fns` and trips the fuel gate. But the strategy re-emits the
    // loop as a terminating structural `def` (see `transpile`) and supplies its
    // OWN universal proof from the accumulator-decomposition lemma — there is no
    // `__fuel` wrapper to drive. Skipping here would drop the very theorem the
    // strategy exists to close, so the fuel gate does not apply.
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    let calls_fuel_bounded = crate::codegen::common::law_calls_unclassified_fn(law, &unclassified);
    // A FOREIGN accumulator-fold reference is the same fuel-gate hazard now that
    // the classifier accepts threaded-accumulator inner loops as structural
    // `def`s: a law calling such a fn (other than the one it is verified on) —
    // e.g. `fac(x) => qfac(x, 1)` verified on `fac` — can't close by simple
    // induction (it needs `qfac`'s own accumulator-decomposition lemma). Before
    // the classifier learned the shape, `qfac` was unclassified and the fuel
    // gate above caught it; now it must be caught here. The verified fn is
    // excluded so an accumulator-generalizing law verified ON the fold
    // (`triTR(n, acc) => plus(triSpec(n), acc)`, which DOES close via
    // `induction … generalizing acc`) is not skipped.
    let calls_foreign_acc_fold =
        crate::codegen::common::law_calls_foreign_accumulator_fold(ctx, law, &vb.fn_name);
    let pinned_self_universal = matches!(
        pinned_law_strategy,
        Some(crate::ir::ProofStrategy::FiniteDomainCases { .. })
            | Some(crate::ir::ProofStrategy::TailRecFixedBaseFold { .. })
    );
    // A hand-proof sidecar FORCES the universal statement: the spliced body
    // proves `∀ givens, <when> = true -> <claim>`, so the law must be emitted
    // in universal (not skipped / sample-only) form regardless of the auto
    // gates above. (`recognize_hand_sidecar` keys on the loaded sidecar map; no
    // sidecar => false => byte-identical to before.)
    let has_hand_sidecar = super::law_auto::recognize_hand_sidecar(ctx, vb, law);
    let skip_universal = !has_hand_sidecar
        && (singleton_const_rhs
            || ((calls_fuel_bounded || calls_foreign_acc_fold) && !pinned_self_universal));
    // Oracle v1: the auto-proof matchers compare law.lhs / law.rhs ASTs. For
    // effectful laws the theorem statement has been rewritten to target the
    // lifted fn (BranchPath.root() + oracle args injected); the matchers need to
    // see the same rewritten form or they'll miss shapes like
    // `pickOne(root, rnd) == pickOneSpec(root, rnd)` and fall back to `sorry`.
    let law_for_auto_proof = crate::ast::VerifyLaw {
        name: law.name.clone(),
        givens: law.givens.clone(),
        when: law.when.clone(),
        lhs: law_lhs.clone(),
        rhs: law_rhs.clone(),
        sample_guards: law.sample_guards.clone(),
    };
    // A recognized EASY conditional comparison-bridge `when`-law (`prop_70
    // leSucc`) is proven UNIVERSALLY by the dedicated bridge emit, so its
    // theorem statement drops the sampled-domain disjunctions (`omit_domain`)
    // and is classed `universal`. Restricted to non-refinement-lifted laws (the
    // lifted path already drops the domain through the quantifier type and uses
    // its own premise handling). Must agree with the proof emitter — both key on
    // the same recognizer.
    let conditional_universal = law.when.is_some()
        && lifted_vars.is_empty()
        && (
            // A hand-proof sidecar proves the true-universal `∀ givens, <when> =
            // true -> claim`, so drop the sampled domain and class it universal
            // (statement and spliced proof stay in lockstep).
            has_hand_sidecar
            || super::law_auto::recognize_conditional_comparison_bridge(&law_for_auto_proof, ctx)
            || super::law_auto::recognize_conditional_inductive_generic(
                vb,
                &law_for_auto_proof,
                ctx,
            )
            // A `when`-premised `NonlinearNonneg` law (the Newton-Raphson
            // factor-sign guards) is proved UNIVERSALLY by the generic
            // `aver_int_order` step, so its statement drops the sampled
            // domain (`omit_domain`) to `∀ givens, <when> = true -> claim`
            // and is classed `universal`. The proof emit keys on the same
            // pin, so statement and proof agree. Credit stays fail-closed:
            // the `#print axioms` whitelist still decides, and the
            // `first | (…) | sorry` floor can never be credited.
            || matches!(
                pinned_law_strategy,
                Some(crate::ir::ProofStrategy::NonlinearNonneg { .. })
            )
            // Keystone — the non-recursive laws-as-lemmas composition (`grind`
            // over the earlier-sibling pool). Drops the sampled domain to the
            // true `∀ givens, <when> = true -> claim`; the proof emit keys on the
            // same recognizer, and the speculative probe commits the universal
            // only when `grind`+pool actually closes (else bounded fallback).
            || super::law_auto::recognize_pool_composition_generic(vb, &law_for_auto_proof, ctx)
            // Multi-citation composition (the K5 reciprocal bucket bound): the
            // goal is supplied by APPLYING an earlier universal whose premises
            // are themselves discharged from an earlier universal's conclusion.
            // Drops the sampled domain to the true `∀ givens, <when> = true ->
            // claim`; the proof emit keys on the same recognizer, and the
            // speculative probe commits the universal only when the orchestration
            // actually closes (else bounded fallback).
            || super::law_auto::recognize_multicite_composition(vb, &law_for_auto_proof, ctx)
            // Finite bounded-Int-domain law (the K5 reciprocal seed table): a
            // single `Int` given guarded into `[LO, HI)`, proven universally by
            // pure-kernel enumeration (`decide` over a bounded-`Nat` forall).
            // The emit replaces the theorem with the `Prop`-hypothesis universal
            // form, so dropping the sampled domain here keeps them in lockstep.
            || super::law_auto::recognize_finite_int_domain(vb, &law_for_auto_proof, ctx)
            // Interval-monotonicity law (the K5 reciprocal table bucket bound):
            // an affine-in-interval-var magnitude bound over the exact-rational
            // order, proven universally by the rational interval-monotonicity
            // argument. The emit replaces the theorem with the `Prop`-hypothesis
            // universal form, so dropping the sampled domain keeps them aligned.
            || super::law_auto::recognize_interval_monotonicity(vb, &law_for_auto_proof, ctx)
            // Transparent arithmetic premise chain: the `when` premise cites
            // citable transparent arithmetic law predicates, and the proof
            // unfolds those bodies before `split at h_when <;> omega`. The
            // statement and proof share this recognizer so the sampled domain is
            // dropped only for shapes the deterministic arm owns.
            || super::law_auto::recognize_transparent_chain(vb, &law_for_auto_proof, ctx)
            // Exact-rational order facts about an opaque unary `Fraction` cone fn
            // `F` (the K5 signed power of two): its POSITIVITY (`F(k).top > 0 &&
            // F(k).bottom > 0`), its AT-LEAST-ONE (`when k >= 0 -> isNonNeg(minus(
            // F(k), oneFraction))`), and the all-exponent MONOTONICITY (`isNonNeg
            // (minus (F E_hi) (F E_lo))` under `E_lo <= E_hi`). The first two cite
            // the recursive-positivity pool law; the monotonicity is laws-as-lemmas
            // composition citing `F`'s homomorphism + positivity + `>= 1` laws,
            // chained through the generic Fraction order kit (`F` opaque). Each
            // emit replaces the theorem with the universal form, so dropping the
            // sampled domain keeps statement and proof aligned.
            || super::law_auto::recognize_frac_positivity(vb, &law_for_auto_proof, ctx)
            || super::law_auto::recognize_frac_geone(vb, &law_for_auto_proof, ctx)
            || super::law_auto::recognize_frac_monotone_compose(vb, &law_for_auto_proof, ctx)
            || super::law_auto::recognize_monotone_reflect(vb, &law_for_auto_proof, ctx)
            || super::law_auto::recognize_magnitude_bracket_reflect(
                vb,
                &law_for_auto_proof,
                ctx,
            )
            // General recursive-monotonicity law (`f(LO) <= f(HI)` for any pure
            // recursive `Int -> Int` `f` under `LO <= HI`): proven universally by
            // the shared recursive-mono kit (`f.induct` positivity + monotonicity)
            // and a one-line citation. The emit replaces the theorem with the
            // universal form, so dropping the sampled domain keeps statement and
            // proof aligned.
            || super::law_auto::recognize_recursive_monotone(vb, &law_for_auto_proof, ctx)
            // Content-blind homomorphism law (`subject(OP1(a, b)) = OP2(subject(a),
            // subject(b))` for a recursive subject, OP1/OP2 captured from the AST):
            // the guarded power-of-two case carries a `when m >= 0; n >= 0` premise
            // and is proven universally by the de-risked `subject.induct` skeleton.
            // The emit replaces the theorem with the universal form, so dropping the
            // sampled domain keeps statement and proof aligned. (The unconditional
            // structural case — a list-length homomorphism — has no sampled domain to
            // drop and is classed universal by the default unconditional path.)
            || super::law_auto::recognize_homomorphism(vb, &law_for_auto_proof, ctx)
            // Nested-Euclidean-floor collapse (`floor (floor a d) e = floor a
            // (d * e)`, positive divisors): proven universally in pure core by
            // `Int.ediv_ediv_of_nonneg`. The emit replaces the theorem with the
            // universal form, so dropping the sampled domain keeps statement and
            // proof aligned.
            || super::law_auto::recognize_nested_floor(&law_for_auto_proof, ctx)
            // Euclidean-floor arithmetic: shared positive-factor cancellation
            // (`floor (a * c) (d * c) = floor a d`) and bounded-remainder
            // absorption (`floor (d * q + r) d = q`), proven universally in pure
            // core. Each emit replaces the theorem with the universal form, so
            // dropping the sampled domain keeps statement and proof aligned.
            || super::law_auto::recognize_cancel_common_factor(vb, &law_for_auto_proof, ctx)
            || super::law_auto::recognize_absorb_remainder(vb, &law_for_auto_proof, ctx)
            // Rational-order chaining law (the reciprocal-magnitude composition,
            // Lemma 8.2.4): the conclusion is the Fraction order fact `isNonNeg
            // (minus (pow2Signed BIG) A)`, proven universally by chaining the
            // scaled-bound / envelope / placement premises through the generic
            // `frac_le_trans` kit and citing the signed power-of-two
            // monotonicity + homomorphism pool laws. The emit replaces the
            // theorem with the universal form, so dropping the sampled domain
            // keeps statement and proof aligned.
            || super::law_auto::recognize_frac_order_chain(vb, &law_for_auto_proof, ctx)
            // Generic rational-order transitivity-chain law (Lemma 8.1.1, the
            // two-step reciprocal-error bound): the conclusion is a STRICT
            // Fraction comparison `lessThan L R` whose premises spell out a
            // 1..3-link comparison chain linking the endpoints, closed
            // universally by the self-contained order kit plus a fixed
            // have-sequence assembler. The emit replaces the theorem with the
            // universal form, so dropping the sampled domain keeps statement
            // and proof aligned.
            || super::law_auto::recognize_frac_order_transitivity(vb, &law_for_auto_proof, ctx)
            // Triangle-sum law (the rounded Newton-Raphson reciprocal step): a
            // strict `absFraction`-of-a-three-term-sum bound over the exact-
            // rational order, proven universally by the generic triangle kit
            // (`tri_sum3`) citing the two rounding bound universals. The emit
            // replaces the theorem with the universal form, so dropping the
            // sampled domain keeps statement and proof aligned.
            || super::law_auto::recognize_triangle_sum(vb, &law_for_auto_proof, ctx)
            // Validated-wrapper correctness (the Theorem-2 shape): an error-
            // checking wrapper that returns `Result.Ok(core(…))` on valid input.
            // The proof body unfolds only the wrapper and closes by reflexivity
            // on the shared opaque core, so the statement drops its sampled
            // domain to the true `∀ givens, <when> = true -> claim` universal.
            || super::law_auto::recognize_validated_wrapper(vb, &law_for_auto_proof, ctx)
            // Mathlib BREAK-GLASS (`--allow-mathlib` only, entry-module only): a
            // walling `when`-law no core recognizer above claimed is emitted in
            // true-universal form and closed by the generic Mathlib portfolio.
            // LAST in the OR and gated on the opt-in flag, so the default path is
            // byte-identical and a core-claimed law keeps its core (non-Mathlib)
            // proof. Kept in lockstep with the `emit_mathlib_break_glass_law` arm
            // (also last in the proof cascade).
            || super::law_auto::recognize_mathlib_break_glass(ctx, &law_for_auto_proof)
        );
    if !quant_params.is_empty() && !skip_universal {
        lines.extend(emit_verify_law_support_theorems(
            vb,
            law,
            ctx,
            &theorem_base,
        ));
        let theorem_parts = law_theorem_parts(
            law,
            ctx,
            &theorem_base,
            &lhs_template,
            &rhs_template,
            when_template.as_deref(),
            &lifted_vars,
            conditional_universal,
        );
        // Statement-class marker — the channel `aver proof --check`'s
        // `universal` metric keys on (see `LAW_CLASS_MARKER_PREFIX`).
        // Recorded HERE because this is where the statement was built:
        // `bounded_domain` says whether sampled-domain disjunction
        // premises bound the claim to the finite sample domain. For a
        // `replaces_theorem` auto-proof the strategy emits its own
        // (universal-form) statement; keeping this class for it is
        // conservative — a mislabel can only withhold credit, never
        // grant it. ONE exception flips the class the other way:
        // `FloorDivWindow` replaces the bounded statement with the
        // TRUE universal form `∀ givens, <when> = true -> claim`
        // (validated emission — the rendered file contains no
        // statement bounded by sampled domains for this law), so the
        // marker says `universal`. Credit stays fail-closed: the
        // `#print axioms` whitelist still decides, and a sorry'd or
        // native_decide'd proof can never be credited.
        // Strategies that emit their OWN true-universal statement
        // (`replaces_theorem`) and so must override the bounded-domain
        // default class: `FloorDivWindow` and `TailRecFixedBaseFold`
        // (the `qexp` tail-recursive-fold shape). Both render a genuine
        // `∀ givens, ... = ...` theorem named `theorem_base` and route
        // through the `emit_verify_law_forall_auto_proof` path below.
        let floor_window_universal = matches!(
            pinned_law_strategy,
            Some(crate::ir::ProofStrategy::FloorDivWindow { .. })
                | Some(crate::ir::ProofStrategy::TailRecFixedBaseFold { .. })
        );
        // The marker carries a THIRD field: the `fn.law` identity label
        // (`format!("{fn}.{law}")`). The file-level audit reads it to key the
        // per-law proof manifest on the stable, edit-robust identity instead
        // of reverse-parsing the (mangled, `_law_`-ambiguous) theorem name.
        // Additive: the marker parser consumes only the first two fields, so
        // an older reader ignores it.
        lines.push(format!(
            "{}{} {} {}.{}",
            super::LAW_CLASS_MARKER_PREFIX,
            theorem_base,
            if theorem_parts.iter().any(|part| part.bounded_domain) && !floor_window_universal {
                super::LAW_CLASS_BOUNDED_DOMAIN
            } else {
                super::LAW_CLASS_UNIVERSAL
            },
            vb.fn_name,
            law.name,
        ));
        // (Removed: refinement_auto_proof — Aver-specific bypass.
        // Refinement-lifted laws now flow through law_auto via the
        // IR-pinned `ProofStrategy::LinearArithmetic { lifted: true }`.
        // The lowerer detects the `Refined(value = given)` carrier
        // shape and pins the strategy; emit_simp_omega_from_ir
        // skips by_cases when lifted=true. Step 30 retired this
        // separate code path so all laws go through one dispatch.)
        if floor_window_universal
            && let Some(auto_proof) = emit_verify_law_forall_auto_proof(
                vb,
                &law_for_auto_proof,
                ctx,
                verify_mode,
                &theorem_base,
                &quant_params,
                &theorem_parts[0].prop,
            )
        {
            lines.extend(auto_proof.support_lines);
            if !auto_proof.replaces_theorem {
                lines.push(format!(
                    "theorem {} : ∀ {}, {} := by",
                    theorem_base, quant_params, theorem_parts[0].prop
                ));
            }
            lines.extend(auto_proof.body.render_body());
        } else {
            // Support lines (e.g. shared `{theorem_base}_digit_pred` private
            // theorems) are keyed on the shared `theorem_base`, so every part's
            // proof references the SAME names. Collect them across all parts,
            // dedup identical lines preserving order, and emit once before the
            // parts — emitting per-first-part only would drop later parts' refs
            // and emitting per-part would redeclare the shared names.
            let mut part_bodies: Vec<Vec<String>> = Vec::with_capacity(theorem_parts.len());
            let mut support_lines: Vec<String> = Vec::new();
            // Past the chunk edge a single part can still hold ~512 rcases
            // leaves; give the partitioned theorems the same per-theorem
            // heartbeats budget the `_checked_domain` parts use (scoped `in`,
            // never file-wide). A single (unpartitioned) theorem keeps its
            // byte-identical pre-chunking header.
            let partitioned = theorem_parts.len() > 1;
            for part in &theorem_parts {
                let law_for_part = crate::ast::VerifyLaw {
                    givens: part.law.givens.clone(),
                    ..law_for_auto_proof.clone()
                };
                let mut body: Vec<String> = Vec::new();
                let header_prefix = if partitioned {
                    "set_option maxHeartbeats 800000 in\n"
                } else {
                    ""
                };
                if let Some(auto_proof) = emit_verify_law_forall_auto_proof(
                    vb,
                    &law_for_part,
                    ctx,
                    verify_mode,
                    &theorem_base,
                    &quant_params,
                    &part.prop,
                ) {
                    if auto_proof.replaces_theorem {
                        // The strategy emitted a part-specific theorem statement
                        // inside its support lines; it is not shared across parts
                        // and must stay in this part's body.
                        body.extend(auto_proof.support_lines);
                    } else if partitioned {
                        // Multiple parts share `{theorem_base}`-keyed support
                        // theorems; collect them once and dedup the IDENTICAL
                        // declarations each part re-emits. (Only safe across parts:
                        // a single part's support stack may legitimately repeat a
                        // tactic line — `unfold F`, `rw [F.eq_def, …]` — inside
                        // distinct theorems, which a line-level dedup would wrongly
                        // strip, so the unpartitioned arm below keeps them verbatim.)
                        for line in auto_proof.support_lines {
                            if !support_lines.contains(&line) {
                                support_lines.push(line);
                            }
                        }
                        body.push(format!(
                            "{}theorem {} : ∀ {}, {} := by",
                            header_prefix, part.name, quant_params, part.prop
                        ));
                    } else {
                        // Single part: emit the support stack verbatim before the
                        // theorem (no cross-part sharing, so no dedup — see above).
                        body.extend(auto_proof.support_lines);
                        body.push(format!(
                            "{}theorem {} : ∀ {}, {} := by",
                            header_prefix, part.name, quant_params, part.prop
                        ));
                    }
                    body.extend(auto_proof.body.render_body());
                } else {
                    body.push(format!(
                        "{}theorem {} : ∀ {}, {} := by",
                        header_prefix, part.name, quant_params, part.prop
                    ));
                    body.push(
                        "  -- verify law is sampled; universal proof must be provided manually"
                            .to_string(),
                    );
                    body.push("  sorry".to_string());
                }
                part_bodies.push(body);
            }
            lines.extend(support_lines);
            for body in part_bodies {
                lines.extend(body);
            }
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
        let domain_conjuncts: Vec<String> = vb
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
                    |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                    mode.clone(),
                );
                let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
                    right,
                    law,
                    |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                    mode,
                );
                let left_str = emit_expr_legacy(&left_rw, ctx, None);
                // Model-vs-ground-truth: same literalization as the per-case
                // `_sample_N` theorems below (see the comment there); this
                // arm is reached only for non-refinement-lifted laws, where
                // the carrier-typed VM literal matches the statement type.
                let right_str =
                    super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
                        .unwrap_or_else(|| emit_expr_legacy(&right_rw, ctx, None));
                if let Some(guard) = law.sample_guards.get(idx) {
                    // Int-ascribed guard rendering — see `emit_sample_guard`
                    // (a bare numeral premise with subtraction elaborates as
                    // truncated `Nat` arithmetic and can change the
                    // proposition).
                    format!(
                        "({} = true -> {} = {})",
                        emit_sample_guard(guard, ctx),
                        left_str,
                        right_str
                    )
                } else {
                    format!("({} = {})", left_str, right_str)
                }
            })
            .collect::<Vec<_>>();
        // Mode-independent chunking decision. The `maxRecDepth` wall the
        // NativeDecide comment below describes is NOT specific to
        // `Decidable`-instance synthesis: plain elaboration of the nested-∧
        // STATEMENT recurses once per conjunct too, so a 512-conjunct
        // theorem fails the build identically under `--verify-mode sorry`
        // and `--verify-mode theorem-skeleton` (the proof body is never
        // reached). Every mode therefore emits the same `<name>_part<i>`
        // partition past the 36-conjunct edge; at or below the edge the
        // single-theorem emission is byte-identical to the pre-chunking
        // output in every mode.
        const CHECKED_DOMAIN_CHUNK: usize = 32;
        let checked_domain_statements: Vec<(String, String)> = if domain_conjuncts.len() > 36 {
            domain_conjuncts
                .chunks(CHECKED_DOMAIN_CHUNK)
                .enumerate()
                .map(|(part_idx, chunk)| {
                    (
                        format!("{}_part{}", domain_theorem_name, part_idx + 1),
                        chunk.join(" ∧ "),
                    )
                })
                .collect()
        } else {
            vec![(domain_theorem_name.clone(), domain_conjuncts.join(" ∧ "))]
        };
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
                //
                // GUARDED conjunctions additionally carry one premise
                // implication + decide-coerced guard per conjunct, and
                // elaboration of that chain can blow the default
                // 200_000 `maxHeartbeats` once the given product grows
                // past ~36 conjuncts (tests/fixtures/nr_wall.av's
                // 36-conjunct mulLeTrans sits at the edge). Give big guarded
                // conjunctions a per-THEOREM budget (scoped `in`, never
                // file-wide) — an honestly-false conjunct still fails
                // `native_decide` the same way, it just isn't
                // misreported as a heartbeat timeout.
                //
                // Past that edge a second elaborator limit bites:
                // `Decidable` instance synthesis recurses once per
                // nested ∧, so a given product of a few hundred cases
                // (e.g. 8×8×8 = 512 — tests/fixtures/large_domain_law
                // .av) exceeds the default `maxRecDepth` and the WHOLE
                // file fails to build — the caught-sorry floor
                // guarantee dies with it. Chunk big conjunctions into
                // `<name>_part<i>` theorems of at most
                // CHECKED_DOMAIN_CHUNK conjuncts each; the union of
                // the parts states exactly the original conjunction.
                // Single-theorem emission is byte-identical up to the
                // 36-conjunct edge (the corpus max), so existing
                // exports do not move.
                let heartbeats_budget = if law.when.is_some() && vb.cases.len() > 36 {
                    "set_option maxHeartbeats 800000 in\n"
                } else {
                    ""
                };
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!(
                        "{}set_option synthInstance.maxSize 4096 in\ntheorem {} : {} := by native_decide",
                        heartbeats_budget, part_name, part_prop
                    ));
                }
            }
            VerifyEmitMode::Sorry => {
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!("theorem {} : {} := by sorry", part_name, part_prop));
                }
            }
            VerifyEmitMode::TheoremSkeleton => {
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!("theorem {} : {} := by", part_name, part_prop));
                    lines.push("  sorry".to_string());
                }
            }
        }
    }

    let sample_indices = sample_indices(vb.cases.len());
    if sample_indices.len() < vb.cases.len() {
        lines.push(format!(
            "-- aver:samples-capped {}/{}",
            sample_indices.len(),
            vb.cases.len()
        ));
    }
    for idx in sample_indices {
        let (left, right) = &vb.cases[idx];
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
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode.clone(),
        );
        let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode,
        );
        let left_str = emit_expr_legacy(&left_rw, ctx, None);
        // Expected side from VM ground truth: the `_sample_N` theorem is
        // `impl(sample) = spec(sample)` by construction — BOTH sides through
        // the model, vacuously provable when fuel exhaustion collapses both
        // to `default`. With a ground-truth entry it becomes
        // `impl(sample) = <value the program computed>`, which is exactly the
        // claim the sample is meant to pin. Refinement-lifted laws are
        // excluded: their statements quantify over the refined subtype, and a
        // carrier-typed literal would not elaborate — they keep the source
        // spec side (the `--check` panic gate still covers them). Misses
        // (failed/skipped at `aver verify`, Float-carrying values — decimal
        // repr isn't bit-exact — or non-round-tripping shapes) fall back to
        // the source spec side.
        let right_str = if lifted_vars.is_empty() {
            super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
                .unwrap_or_else(|| emit_expr_legacy(&right_rw, ctx, None))
        } else {
            emit_expr_legacy(&right_rw, ctx, None)
        };
        let sample_prop = if let Some(guard) = law.sample_guards.get(idx) {
            // Int-ascribed guard rendering — see `emit_sample_guard` (a bare
            // numeral premise with subtraction elaborates as truncated `Nat`
            // arithmetic and can make the theorem FALSE AS STATED).
            format!(
                "{} = true -> {} = {}",
                emit_sample_guard(guard, ctx),
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

/// The discovery feedback loop, część A: a proved earlier `verify … law`
/// becomes a lemma for later laws. Returns `(theorem_name, "lhs = rhs")` for a
/// law usable as a `simp` rewrite rule, mirroring `emit_verify_law_block`'s
/// name + lhs/rhs template computation EXACTLY (so the referenced name and the
/// orientation-analysis text match the actually-emitted theorem). `None` for
/// shapes that aren't a clean equational rewrite rule: trace-projection LHS
/// (no theorem emitted), a `when` premise (a conditional equation, not a plain
/// rewrite), or a refinement-lifted given (the statement quantifies over a
/// subtype — not a useful rewrite over the carrier). The name is the SAME
/// `<fn>_eq_<spec>` / `<fn>_law_<name>` the block emits, so a later law's
/// `simp [<name>]` resolves against the earlier theorem already in scope.
/// The Lean theorem name `emit_verify_law_block` gives this law's universal
/// theorem — `<fn>_eq_<spec>` for an equational spec law, else `<fn>_law_<name>`.
/// Mirrors the `theorem_base` computation in `emit_verify_law_block` exactly, so
/// a caller can key the cross-file admission set on the same name the block
/// emits even for a law `law_as_lemma_statement` declines to state as a plain
/// rewrite (a `when`-premised universal the keystone closes, cited by name).
pub(crate) fn law_theorem_base(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> String {
    let fn_name = aver_name_to_lean(&vb.fn_name);
    match canonical_spec_ref(&vb.fn_name, law, ctx) {
        Some(spec_ref) => format!(
            "{}_eq_{}",
            fn_name,
            aver_name_to_lean(&spec_ref.spec_fn_name)
        ),
        None => format!("{}_law_{}", fn_name, aver_name_to_lean(&law.name)),
    }
}

pub(crate) fn law_as_lemma_statement(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    // A `when`-premise law is citable as a CONDITIONAL rewrite (`P = true ->
    // lhs = rhs`) — but only once it is proven universally, which the
    // conditional recognizers gate. Before #599 no `when`-law was ever
    // universal, so the pool declined them wholesale; now a proven conditional
    // law is a sound conditional simp lemma. Decline a `when`-law the auto-
    // prover only bounds (it has no universal theorem to cite).
    // A `when`-law is also a sound conditional rewrite when an IR-pinned
    // strategy proves it universally as a clean `∀ givens, <when> = true ->
    // claim` theorem named `<fn>_law_<name>` — the power-of-two homomorphism
    // (`FloorDivWindow`), the Newton-Raphson nonneg/order laws
    // (`NonlinearNonneg`), and the `qexp` fold (`TailRecFixedBaseFold`). Citing
    // those is exactly what the keystone laws-as-lemmas composition needs.
    // (`recognize_pool_composition_generic` is deliberately NOT consulted here —
    // it calls back into this fn, which would recurse.)
    //
    // SOUNDNESS: this gate is permissive at the STATEMENT level and fail-closed at
    // the CREDIT level. It keys on the IR strategy TYPE, not on whether that law's
    // emit actually closed without `sorry` — a pinned law whose `first | (…) |
    // sorry` portfolio fell to its floor still has a (sorry-carrying) theorem to
    // cite. That cannot launder credit: `sorry` propagates transitively, so any
    // proof that cites a sorry-floored law inherits `sorryAx`, and the final
    // `--check` axiom whitelist (blacklist `{ofReduceBool, sorryAx}`) refuses it
    // universal. The worst case is a previously-universal citing law LOSING credit
    // (a regression caught by the proof corpus), never a false universal.
    let pinned_when_universal = matches!(
        ctx.law_target_fn_id(&vb.fn_name)
            .and_then(|fn_id| ctx
                .proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name))
            .map(|t| &t.strategy),
        Some(
            crate::ir::ProofStrategy::FloorDivWindow { .. }
                | crate::ir::ProofStrategy::NonlinearNonneg { .. }
                | crate::ir::ProofStrategy::TailRecFixedBaseFold { .. }
        )
    );
    if law.when.is_some()
        && !(super::law_auto::recognize_conditional_comparison_bridge(law, ctx)
            || super::law_auto::recognize_conditional_inductive_generic(vb, law, ctx)
            // The exact-rational at-least-one (`F(k) >= 1` for `k >= 0`) and
            // monotonicity (`F(LO) <= F(HI)` for `LO <= HI`) conditional laws are
            // proven universally by their dedicated composition rungs, so they are
            // sound conditional rewrites a later law (the monotonicity citing the
            // at-least-one) can cite.
            || super::law_auto::recognize_frac_geone(vb, law, ctx)
            || super::law_auto::recognize_frac_monotone_compose(vb, law, ctx)
            || super::law_auto::recognize_monotone_reflect(vb, law, ctx)
            || super::law_auto::recognize_magnitude_bracket_reflect(vb, law, ctx)
            // The finite bounded-Int-domain law (`∀ i, LO ≤ i → i < HI → P i`,
            // proven by pure-kernel `decide` enumeration) and the interval-
            // monotonicity law (`subject(d, v, e)` over an interval, proven by the
            // rational interval argument) both have a clean `∀ givens, <when> =
            // true -> claim` universal theorem named `<fn>_law_<name>`, so they are
            // sound conditional rewrites a later law can cite. The keystone's
            // laws-as-lemmas composition (the K5 reciprocal bucket bound) cites
            // exactly this pair: the table seed bound and the endpoint
            // interval-magnitude bound.
            || super::law_auto::recognize_finite_int_domain(vb, law, ctx)
            || super::law_auto::recognize_interval_monotonicity(vb, law, ctx)
            || super::law_auto::recognize_transparent_chain(vb, law, ctx)
            || super::law_auto::recognize_validated_wrapper(vb, law, ctx)
            || pinned_when_universal)
    {
        return None;
    }
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        return None;
    }
    // A referenceable lemma must actually be EMITTED as a `∀`-theorem with the
    // name we synthesize below — otherwise a later law's `simp [<name>]` hits
    // an unknown identifier and fails the whole file's build. Decline exactly
    // the cases `emit_verify_law_block` skips the universal theorem:
    //   - no givens → no quantified theorem (only concrete samples);
    //   - `skip_universal` — a singleton-domain const-RHS law (vacuous/false
    //     universal) or a law calling a fuel-bounded recursive helper (the
    //     auto-prover can't close it, only per-sample lemmas are emitted).
    // Mirrors the guard in `emit_verify_law_block` (kept in sync by the
    // część A regression test); a false decline only forgoes a helper, never
    // references a missing theorem.
    if law.givens.is_empty() {
        return None;
    }
    let ir_strategy_closes_const_rhs = ctx
        .law_target_fn_id(&vb.fn_name)
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
                    | crate::ir::ProofStrategy::SimpOverLemmas(_)
                    | crate::ir::ProofStrategy::BackendDispatch
                    | crate::ir::ProofStrategy::Sorry
            )
        });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    if singleton_const_rhs || crate::codegen::common::law_calls_unclassified_fn(law, &unclassified)
    {
        return None;
    }
    let fn_name = aver_name_to_lean(&vb.fn_name);
    let law_name = aver_name_to_lean(&law.name);
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, ctx);
    let theorem_base = match &spec_ref {
        Some(spec_ref) => format!(
            "{}_eq_{}",
            fn_name,
            aver_name_to_lean(&spec_ref.spec_fn_name)
        ),
        None => format!("{}_law_{}", fn_name, law_name),
    };
    let law_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let law_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    // A refinement-lifted given would change the quantifier type (the theorem
    // reads `∀ a : Natural, …`); such a statement isn't a plain rewrite rule
    // over the carrier, so decline rather than mis-orient it.
    for given in &law.givens {
        if crate::codegen::common::refinement_lift_for_given(
            &given.name,
            &given.type_name,
            &law_lhs,
            &law_rhs,
            ctx,
        )
        .is_some()
        {
            return None;
        }
    }
    let lhs = emit_expr_legacy(&law_lhs, ctx, None);
    let rhs = emit_expr_legacy(&law_rhs, ctx, None);
    // A `when`-law is cited as the conditional rewrite the auto-prover proved:
    // `<premise> = true -> lhs = rhs`, matching the `omit_domain` theorem.
    match &law.when {
        Some(when) => {
            let premise = emit_expr_legacy(when, ctx, None);
            Some((theorem_base, format!("{premise} = true -> {lhs} = {rhs}")))
        }
        None => Some((theorem_base, format!("{lhs} = {rhs}"))),
    }
}

#[derive(Clone)]
struct LawTheoremPart {
    name: String,
    prop: String,
    bounded_domain: bool,
    law: VerifyLaw,
}

fn sample_indices(total: usize) -> Vec<usize> {
    if total <= LAW_SAMPLE_CAP || LAW_SAMPLE_CAP == 0 {
        return (0..total).collect();
    }
    if LAW_SAMPLE_CAP == 1 {
        return vec![0];
    }
    (0..LAW_SAMPLE_CAP)
        .map(|i| i * (total - 1) / (LAW_SAMPLE_CAP - 1))
        .collect()
}

fn emitted_domain_values(
    law: &VerifyLaw,
    lifted_vars: &std::collections::HashMap<String, String>,
) -> Vec<Option<Vec<Spanned<Expr>>>> {
    law.givens
        .iter()
        .map(|given| {
            (!lifted_vars.contains_key(&given.name)).then(|| law_given_domain_values(&given.domain))
        })
        .collect()
}

fn bounded_law_slice(
    domain_values: &[Option<Vec<Spanned<Expr>>>],
) -> Option<(usize, usize, Vec<Spanned<Expr>>)> {
    let mut total_cases = 1usize;
    let mut largest: Option<(usize, Vec<Spanned<Expr>>)> = None;
    for (idx, values) in domain_values.iter().enumerate() {
        let Some(values) = values else { continue };
        total_cases = total_cases.saturating_mul(values.len());
        if largest
            .as_ref()
            .is_none_or(|(_, current)| values.len() > current.len())
        {
            largest = Some((idx, values.clone()));
        }
    }
    let (idx, values) = largest?;
    if values.is_empty() {
        return None;
    }
    let other_cases = (total_cases / values.len()).max(1);
    let chunk_by_value = if values.len() > BOUNDED_LAW_DOMAIN_VALUE_EDGE {
        BOUNDED_LAW_DOMAIN_VALUE_EDGE
    } else if total_cases > BOUNDED_LAW_DOMAIN_CASE_EDGE {
        (BOUNDED_LAW_DOMAIN_CASE_EDGE / other_cases).max(1)
    } else {
        values.len()
    };
    (chunk_by_value < values.len()).then_some((idx, chunk_by_value, values))
}

fn law_with_sliced_domain(
    law: &VerifyLaw,
    given_idx: usize,
    values: &[Spanned<Expr>],
) -> VerifyLaw {
    let mut part = law.clone();
    part.givens[given_idx].domain = VerifyGivenDomain::Explicit(values.to_vec());
    part
}

#[allow(clippy::too_many_arguments)]
fn law_theorem_parts(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    lhs_template: &str,
    rhs_template: &str,
    when_template: Option<&str>,
    lifted_vars: &std::collections::HashMap<String, String>,
    omit_domain: bool,
) -> Vec<LawTheoremPart> {
    // `omit_domain` (a recognized conditional-comparison-bridge `when`-law)
    // drops the sampled-domain disjunctions, so the statement is the
    // TRUE-universal `∀ givens, when = true -> claim`. The resulting
    // `bounded_domain = false` keeps the single-part path (no domain chunking —
    // there is no large disjunction to split) and flips the law-class marker to
    // `universal`.
    let (prop, bounded_domain) = law_theorem_prop(
        law,
        ctx,
        lhs_template,
        rhs_template,
        when_template,
        lifted_vars,
        omit_domain,
    );
    let single = || {
        vec![LawTheoremPart {
            name: theorem_base.to_string(),
            prop: prop.clone(),
            bounded_domain,
            law: law.clone(),
        }]
    };
    if !bounded_domain {
        return single();
    }
    let domain_values = emitted_domain_values(law, lifted_vars);
    let Some((given_idx, chunk_size, values)) = bounded_law_slice(&domain_values) else {
        return single();
    };
    values
        .chunks(chunk_size)
        .enumerate()
        .map(|(part_idx, chunk)| {
            let part_law = law_with_sliced_domain(law, given_idx, chunk);
            let (part_prop, part_bounded) = law_theorem_prop(
                &part_law,
                ctx,
                lhs_template,
                rhs_template,
                when_template,
                lifted_vars,
                false,
            );
            LawTheoremPart {
                // Manifest-vs-manifest collision hazard: a `<base>_part<N>`
                // chunk name can in principle collide with a sibling law
                // literally named `<base>_part<N>`. This is fail-closed today —
                // the file-level collision guard demotes a
                // duplicated theorem name, and the audit keys `bounded_laws` on
                // each part's class marker (direct-lookup-first), so a real law
                // named `<base>_partN` is never folded onto a phantom base. A
                // proactive name-uniqueness guard at emission is deferred.
                name: format!("{}_part{}", theorem_base, part_idx + 1),
                prop: part_prop,
                bounded_domain: part_bounded,
                law: part_law,
            }
        })
        .collect()
}

/// Build the law theorem's statement body. Returns `(prop, bounded_domain)`:
/// `bounded_domain` is `true` iff sampled-domain disjunction premises
/// (`a = 0 ∨ a = 1 ∨ …`) were prepended — the statement then only claims the
/// law over the finite sample domain, NOT universally. This flag is the
/// single source of truth for the `-- aver:law-class` marker the caller
/// emits (see `LAW_CLASS_MARKER_PREFIX`): the checker's `universal` metric
/// keys on it instead of re-deriving the class from names or statements.
/// A `when`-premise alone (`… = true ->`) does NOT bound the statement —
/// it is a conditional but still universally quantified claim (the
/// refinement-lifted case, where every given's domain premise is dropped).
///
/// `omit_domain` is the universal statement mode for a conditional law the
/// auto-prover closes through induction + the laws-as-lemmas pool: it renders
/// `∀ givens, <when> = true -> claim` by skipping the sampled-domain
/// disjunctions entirely, so the universal statement differs from the bounded
/// manifest theorem only by those premises. A law the conditional recognizers
/// decline passes `false` and keeps the bounded sampled-domain statement.
pub(in crate::codegen::lean) fn law_theorem_prop(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    lhs_template: &str,
    rhs_template: &str,
    when_template: Option<&str>,
    lifted_vars: &std::collections::HashMap<String, String>,
    omit_domain: bool,
) -> (String, bool) {
    let mut premises = Vec::new();
    let when_redundant_with_lifts = law
        .when
        .as_ref()
        .map(|w| {
            crate::codegen::common::when_is_redundant_with_refinement_lifts(w, lifted_vars, ctx)
        })
        .unwrap_or(false);
    if law.when.is_some() && !omit_domain {
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
    let bounded_domain = !premises.is_empty();
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
    let prop = if premises.is_empty() {
        conclusion
    } else {
        format!("{} -> {}", premises.join(" -> "), conclusion)
    };
    (prop, bounded_domain)
}

fn law_given_domain_to_lean(domain: &VerifyGivenDomain, ctx: &CodegenContext) -> String {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => format!("{}..{}", start, end),
        VerifyGivenDomain::Explicit(values) => format!(
            "[{}]",
            values
                .iter()
                .map(|v| emit_expr_legacy(v, ctx, None))
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
        [value] => format!("{given_name} = {}", emit_expr_legacy(value, ctx, None)),
        _ => values
            .iter()
            .map(|value| format!("{given_name} = {}", emit_expr_legacy(value, ctx, None)))
            .collect::<Vec<_>>()
            .join(" ∨ "),
    }
}

pub(in crate::codegen::lean) fn law_given_domain_values(
    domain: &VerifyGivenDomain,
) -> Vec<Spanned<Expr>> {
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

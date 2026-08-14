//! ProofIR producer regression tests.
//!
//! `proof_lower::populate_refined_types` + `populate_fn_contracts`
//! are the single source of truth — both Lean and Dafny exporters
//! read `ctx.proof_ir.*` instead of re-classifying. The tests here
//! pin the producer's output shape: for each canonical source
//! pattern, they assert the resulting `RefinedTypeDecl` /
//! `FnContract` carries the expected carrier / predicate / fuel
//! metric / preservation marker / body decomposition.
//!
//! Historical context: through Steps 2-13 these tests cross-checked
//! the new ProofIR producer against the legacy `refinement_info_for`
//! / `analyze_plans` walkers, asserting the producer agreed with the
//! classifier. After Step 18 / 20 retired the consumer-side
//! `analyze_plans` calls the cross-check became a producer-output
//! pin: if the classifier itself changes shape, ProofIR follows, and
//! these tests catch a divergence between the two halves.

use aver::ast::{Spanned, TopLevel, TypeDef};
use aver::codegen::CodegenContext;
use aver::codegen::common::refinement_info_for;
use aver::codegen::recursion::{RecursionPlan, analyze_plans_in_scope};
use aver::ir::proof_ir::{
    DecreaseProof, FuelMetric, Measure, PreservationProof, QuantifierType, RecursionContract,
};
use aver::source::parse_source;

fn build_ctx(src: &str) -> CodegenContext {
    let mut items = parse_source(src).expect("parse");
    // Proof-mode minimal pipeline: rewrite stages off (would alter
    // source-level recursion shapes the classifier matches against).
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            run_tco: true,
            typecheck: Some(aver::ir::TypecheckMode::Full { base_dir: None }),
            run_interp_lower: false,
            run_buffer_build: false,
            run_resolve: false,
            run_last_use: false,
            run_analyze: true,
            run_escape: false,
            run_refinement_lower: true,
            run_interval_analyze: false,
            run_contract_lower: true,
            run_law_lower: true,
            // Build the symbol table alongside proof IR so downstream
            // tests that touch `ctx.symbol_table` see the populated
            // form. Cheap traversal — no analysis.
            run_build_symbols: true,
            dep_modules: &[],
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        },
    );
    let tc = pipeline_result.typecheck.expect("typecheck requested");
    assert!(tc.errors.is_empty(), "source typechecks: {:?}", tc.errors);
    let proof_ir = pipeline_result.proof_ir;
    let mut ctx = aver::codegen::build_context(
        items,
        &tc,
        pipeline_result.analysis.as_ref(),
        "diff".to_string(),
        vec![],
        pipeline_result.symbol_table,
        pipeline_result.resolved_items,
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }
    ctx
}

/// Resolve a top-level fn's `FnContract` by bare name. After the
/// FnKey → FnId migration the contracts map is keyed by opaque
/// `FnId`, so tests resolve the identity through the symbol table
/// the same way backends do.
fn fn_contract<'a>(
    ctx: &'a CodegenContext,
    name: &str,
) -> Option<&'a aver::ir::proof_ir::FnContract> {
    let key = aver::ir::FnKey::entry(name);
    let id = ctx.symbol_table.fn_id_of(&key)?;
    ctx.proof_ir.fn_contracts.get(&id)
}

/// Find a `LawTheorem` by entry-scope fn name + law name. Mirror of
/// `fn_contract` for the FnKey → FnId migration: tests resolve the
/// fn identity through the symbol table, then match by opaque id.
fn law_theorem<'a>(
    ctx: &'a CodegenContext,
    fn_name: &str,
    law_name: &str,
) -> Option<&'a aver::ir::proof_ir::LawTheorem> {
    let fn_id = ctx
        .symbol_table
        .fn_id_of(&aver::ir::FnKey::entry(fn_name))?;
    ctx.proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == fn_id && t.law_name == law_name)
}

fn legacy_decision(ctx: &CodegenContext, type_name: &str) -> Option<LegacyDecl> {
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let info = refinement_info_for(type_name, &inputs)?;
    Some(LegacyDecl {
        carrier_type: info.carrier_type.to_string(),
        carrier_field: info.carrier_field.to_string(),
        predicate_param: info.param_name.to_string(),
        predicate_repr: spanned_repr_ast(&inputs, info.predicate, None),
    })
}

#[derive(Debug, PartialEq)]
struct LegacyDecl {
    carrier_type: String,
    carrier_field: String,
    predicate_param: String,
    predicate_repr: String,
}

/// Stable IR-form fingerprint for cross-check. Uses Debug output —
/// the migrated path (Phase E PR 12 Scope A) now stores
/// `Spanned<ResolvedExpr>` in ProofIR, so the cross-check resolves
/// the legacy `Spanned<ast::Expr>` slice through the same
/// `ProofLowerInputs::resolve_expr` the producer uses, then compares
/// IR-form Debug strings on both sides.
fn spanned_repr(expr: &Spanned<aver::ir::hir::ResolvedExpr>) -> String {
    format!("{:?}", expr.node)
}

/// Helper for legacy paths that still hold raw `Spanned<ast::Expr>`.
/// Resolves through the same `inputs.resolve_expr` the IR producer
/// calls so both sides print in the same IR shape for the diff.
fn spanned_repr_ast(
    inputs: &aver::codegen::proof_lower::ProofLowerInputs,
    expr: &Spanned<aver::ast::Expr>,
    scope: Option<&str>,
) -> String {
    let resolved = inputs.resolve_expr(expr, scope);
    format!("{:?}", resolved.node)
}

fn assert_equiv(src: &str, type_names: &[&str]) {
    let ctx = build_ctx(src);
    for &type_name in type_names {
        let legacy = legacy_decision(&ctx, type_name);
        let new = ctx
            .proof_ir
            .refined_types
            .values()
            .find(|d| d.name == type_name);
        match (&legacy, new) {
            (None, None) => continue,
            (Some(_), None) => panic!(
                "legacy classified {} as refined, ProofIR did not",
                type_name
            ),
            (None, Some(_)) => panic!(
                "ProofIR classified {} as refined, legacy did not",
                type_name
            ),
            (Some(l), Some(n)) => {
                assert_eq!(
                    l.carrier_type, n.carrier_type,
                    "carrier_type mismatch for {}",
                    type_name
                );
                assert_eq!(
                    l.carrier_field, n.carrier_field,
                    "carrier_field mismatch for {}",
                    type_name
                );
                assert_eq!(
                    l.predicate_param, n.predicate_param,
                    "predicate_param mismatch for {}",
                    type_name
                );
                assert_eq!(
                    l.predicate_repr,
                    spanned_repr(&n.invariant.expr),
                    "predicate expr mismatch for {}",
                    type_name
                );
            }
        }
    }
}

#[test]
fn natural_refinement_decision_matches_legacy() {
    let src = include_str!("../examples/refinement/natural/natural.av");
    assert_equiv(src, &["Natural"]);
}

#[test]
fn pipeline_populates_symbol_table_when_build_symbols_is_on() {
    // #138 phase E wire-up: when the pipeline runs with
    // `run_build_symbols = true`, the result carries a populated
    // `SymbolTable` and `build_codegen_context` plumbs it into
    // `ctx.symbol_table`. No downstream consumer reads it yet —
    // this test is the contract that the table exists.
    let src = include_str!("../examples/refinement/natural/natural.av");
    let ctx = build_ctx(src);
    let symbols = &ctx.symbol_table;

    // Natural.av declares the `Natural` record + the `fromInt` /
    // `toInt` / `add` / `mul` fns. Look up via `TypeKey` /
    // `FnKey` directly — exercises the resolver path the
    // migration PRs will rely on.
    let nat_id = symbols
        .type_id_of(&aver::ir::TypeKey::entry("Natural"))
        .expect("Natural type id");
    let entry = symbols.type_entry(nat_id);
    assert_eq!(entry.key.name, "Natural");
    assert!(entry.is_product, "Natural is a record (product)");

    let from_int = symbols
        .fn_id_of(&aver::ir::FnKey::entry("fromInt"))
        .expect("fromInt fn id");
    assert_eq!(symbols.fn_entry(from_int).key.name, "fromInt");

    // Result.Ok / Result.Err / Option.Some / Option.None are
    // built-ins, NOT user-declared, so they must NOT appear here.
    // (Future: built-in ctors get their own well-known IDs; for
    // now their absence is the contract.)
    assert!(
        symbols
            .type_id_of(&aver::ir::TypeKey::entry("Result"))
            .is_none()
    );
}

#[test]
fn positive_refinement_decision_matches_legacy() {
    let src = include_str!("../examples/refinement/positive/positive.av");
    assert_equiv(src, &["Positive"]);
}

#[test]
fn int_range_refinement_decision_matches_legacy() {
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    assert_equiv(src, &["IntRange"]);
}

// ── persisted interval / op-class facts on RefinedTypeDecl ──────────
//
// `populate_refined_types` back-fills each `RefinedTypeDecl` with the
// interval analysis's derived bound + per-op classification, reusing
// `interval::analyze`. The fact is queryable on the standard
// refinement-lower path — `build_ctx` runs with
// `run_interval_analyze: false`, so these tests prove the persisted
// fields are populated WITHOUT the `--explain-passes` diagnostic flag.
// This is the home a future carrier-lowering codegen recognizer reads
// via `ctx.proof_ir.refined_types` (TypeId-keyed).

/// Look up the persisted decl for a refined type by source name.
fn refined_decl<'a>(
    ctx: &'a CodegenContext,
    type_name: &str,
) -> &'a aver::ir::proof_ir::RefinedTypeDecl {
    let matches: Vec<_> = ctx
        .proof_ir
        .refined_types
        .values()
        .filter(|d| d.name == type_name)
        .collect();
    assert_eq!(
        matches.len(),
        1,
        "expected exactly one `{type_name}` refined type, got {}",
        matches.len()
    );
    matches[0]
}

/// Persisted op-class for a named op on a decl.
fn persisted_op_class(
    decl: &aver::ir::proof_ir::RefinedTypeDecl,
    op: &str,
) -> aver::ir::interval::OpClass {
    decl.op_classes
        .iter()
        .find(|(name, _)| name == op)
        .map(|(_, c)| *c)
        .unwrap_or_else(|| panic!("op `{op}` not classified; ops = {:?}", decl.op_classes))
}

#[test]
fn int_range_persists_two_sided_interval_and_overflow_free_add() {
    use aver::ir::Interval;
    use aver::ir::interval::OpClass;

    let src = include_str!("../examples/refinement/int_range/int_range.av");
    let ctx = build_ctx(src);
    let decl = refined_decl(&ctx, "IntRange");

    // The `Bool.and(n >= 0, n <= 100)` guard yields the two-sided
    // `[0, 100]` enclosure, persisted as `Some`.
    assert_eq!(
        decl.interval,
        Some(Interval::between(0, 100)),
        "IntRange's [0,100] guard is recognized and persisted on the decl"
    );

    // `add` of two `[0,100]` values has intermediate `[0,200]`, which
    // fits i64 — the native-i64 candidate. (The `fromInt` guard still
    // re-validates the [0,100] bound; OverflowFree is about the
    // intermediate, not the result.)
    assert_eq!(
        persisted_op_class(decl, "add"),
        OpClass::OverflowFree,
        "IntRange.add intermediate [0,200] fits i64 → persisted OverflowFree"
    );

    // The persisted-fact recognizer (the gate a later carrier-lowering
    // slice trusts, read off the decl WITHOUT the diagnostic flag):
    // two-sided i64-fitting interval + every op OverflowFree → eligible.
    assert!(
        decl.raw_i64_eligible(),
        "IntRange [0,100], `add` OverflowFree → persisted decl is raw-i64-eligible"
    );
}

#[test]
fn natural_persists_one_sided_interval_and_unbounded_ops() {
    use aver::ir::interval::{Bound, OpClass};

    let src = include_str!("../examples/refinement/natural/natural.av");
    let ctx = build_ctx(src);
    let decl = refined_decl(&ctx, "Natural");

    // `n >= 0` is a recognized one-sided shape → `[0, +inf]`, persisted
    // as `Some` (recognized, even though it is not two-sided).
    let interval = decl
        .interval
        .expect("Natural's n>=0 guard is recognized and persisted");
    assert_eq!(interval.lo, Bound::Finite(0));
    assert_eq!(interval.hi, Bound::PosInf, "Natural is [0,+inf]");

    // `[0,+inf]` operands give `[0,+inf]` intermediates — no derivable
    // finite bound, so every op persists `Unbounded` (must stay bignum).
    assert_eq!(persisted_op_class(decl, "add"), OpClass::Unbounded);
    assert_eq!(persisted_op_class(decl, "mul"), OpClass::Unbounded);

    // One-sided interval (open upper bound) → the persisted decl is NOT
    // raw-i64-eligible: the carrier could not fit a machine word.
    assert!(
        !decl.raw_i64_eligible(),
        "Natural's open upper bound → persisted decl NOT raw-i64-eligible"
    );
}

#[test]
fn persisted_classification_equals_explain_passes_interval_analysis() {
    // The persisted decl fields must be IDENTICAL to what the
    // `--explain-passes` diagnostic path (`run_interval_analyze: true`,
    // `PipelineResult.interval_analysis`) reports for the same program.
    // Both paths call the one `interval::analyze`; this test pins that
    // there is no divergence between the persisted home and the
    // diagnostic flag.
    let src = include_str!("../examples/refinement/int_range/int_range.av");

    // Persisted path: standard refinement-lower (interval_analyze OFF).
    let ctx = build_ctx(src);

    // Diagnostic path: same pipeline shape but interval_analyze ON,
    // reading the standalone `IntervalAnalysisResult`.
    let mut items = parse_source(src).expect("parse");
    let diag = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            run_tco: true,
            typecheck: Some(aver::ir::TypecheckMode::Full { base_dir: None }),
            run_interp_lower: false,
            run_buffer_build: false,
            run_resolve: false,
            run_last_use: false,
            run_analyze: true,
            run_escape: false,
            run_refinement_lower: true,
            run_interval_analyze: true,
            run_contract_lower: false,
            run_law_lower: false,
            run_build_symbols: true,
            dep_modules: &[],
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        },
    );
    let diag_analysis = diag
        .interval_analysis
        .expect("interval analysis present when run_interval_analyze=true");

    // For every persisted decl, the diagnostic result keyed by the same
    // TypeId must agree on both the interval and the per-op classes.
    assert!(
        !ctx.proof_ir.refined_types.is_empty(),
        "IntRange must be lifted into refined_types"
    );
    for (type_id, decl) in &ctx.proof_ir.refined_types {
        let per_type = diag_analysis
            .types
            .get(type_id)
            .expect("every persisted decl has a diagnostic counterpart by TypeId");

        // Persisted `interval` is `Some(..)` exactly when the diagnostic
        // recognized the shape (`interval_known`), and the value matches.
        let expected_interval = per_type.interval_known.then_some(per_type.interval);
        assert_eq!(
            decl.interval, expected_interval,
            "persisted interval for `{}` diverges from --explain-passes",
            decl.name
        );
        assert_eq!(
            decl.op_classes, per_type.ops,
            "persisted op classes for `{}` diverge from --explain-passes",
            decl.name
        );
    }
}

#[test]
fn non_refinement_records_dont_appear_in_proof_ir() {
    // A non-refinement record (multiple fields, no smart constructor)
    // must not show up in `refined_types`. The legacy walker would
    // return `None`; ProofIR must agree.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         record Point\n\
         \x20   x: Int\n\
         \x20   y: Int\n";
    let ctx = build_ctx(src);
    assert!(
        ctx.proof_ir.refined_types.is_empty(),
        "multi-field record must not be lifted, got: {:?}",
        ctx.proof_ir.refined_types.keys().collect::<Vec<_>>()
    );
    // Sanity: legacy walker also rejects.
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    assert!(refinement_info_for("Point", &inputs).is_none());
    // Don't trigger unused-import warnings on the TypeDef / TopLevel
    // imports — they're carried for readers cross-referencing
    // proof_lower internals.
    let _: Option<&TypeDef> = ctx.items.iter().find_map(|i| match i {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
}

#[test]
fn inhabitation_witness_matches_legacy_for_each_example() {
    // The legacy witness picker lives in `dafny/toplevel.rs::
    // refinement_witness_for` and isn't pub-exported. Mirror its
    // expected output for each flagship example so a divergence
    // surfaces here even though we can't call the legacy fn
    // directly. The pre-Step-2 legacy emit (from main as of the
    // ProofIR branch start) produced:
    //   Natural   -> witness 0  (verify case fromInt(0) => Ok)
    //   Positive  -> witness 1  (predicate-eval fallback)
    //   IntRange  -> witness 0  (verify case fromInt(0) => Ok)
    let cases: &[(&str, &str, &str)] = &[
        (
            include_str!("../examples/refinement/natural/natural.av"),
            "Natural",
            "0",
        ),
        (
            include_str!("../examples/refinement/positive/positive.av"),
            "Positive",
            "1",
        ),
        (
            include_str!("../examples/refinement/int_range/int_range.av"),
            "IntRange",
            "0",
        ),
    ];
    for (src, name, expected_witness) in cases {
        let ctx = build_ctx(src);
        let decl = ctx
            .proof_ir
            .refined_types
            .values()
            .find(|d| d.name == *name)
            .unwrap_or_else(|| panic!("{} not lifted in ProofIR", name));
        assert_eq!(
            decl.witness.as_deref(),
            Some(*expected_witness),
            "Inhabitation witness mismatch for {}",
            name
        );
    }
}

#[test]
fn fib_tr_native_contract_matches_legacy_recursion_plan() {
    // fibTR is the canonical IntCountdownGuarded shape:
    //   match n { 0 -> a; _ -> fibTR(n - 1, b, a + b) }
    // The legacy classifier emits `IntCountdownGuarded` with whatever
    // precondition it derives from the single external caller (or
    // empty if no caller is visible in the artifact). ProofIR's
    // FnContract::Native must agree on:
    //  - the bound countdown param name (drives the measure binder)
    //  - precondition arity + per-clause AST (caller-derived
    //    conjuncts in callee variable space)
    //  - PreservationProof::IntCountdownLiteralZero + DecreaseProof::
    //    NatAbsCountdown markers (only valid combo for this shape).
    let src = include_str!("../examples/data/fibonacci.av");
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    let RecursionPlan::IntCountdownGuarded {
        param_index,
        precondition: legacy_precondition,
        ..
    } = plans
        .get("fibTR")
        .unwrap_or_else(|| panic!("fibTR has no classified plan"))
    else {
        panic!(
            "fibTR plan is not IntCountdownGuarded: {:?}",
            plans.get("fibTR")
        );
    };

    let contract =
        fn_contract(&ctx, "fibTR").unwrap_or_else(|| panic!("fibTR has no FnContract in ProofIR"));
    let RecursionContract::Native {
        precondition,
        measure,
        preservation,
        decrease,
        body,
    } = contract
        .recursion
        .as_ref()
        .unwrap_or_else(|| panic!("fibTR FnContract has no recursion"))
    else {
        panic!("fibTR recursion is not Native: {:?}", contract.recursion);
    };

    let fd = ctx
        .items
        .iter()
        .find_map(|item| match item {
            TopLevel::FnDef(fd) if fd.name == "fibTR" => Some(fd),
            _ => None,
        })
        .expect("fibTR FnDef");
    let countdown_param = &fd.params[*param_index].0;

    assert!(
        matches!(measure, Measure::NatAbsInt { param } if param == countdown_param),
        "measure must bind the countdown param, got {:?}",
        measure
    );
    assert!(
        matches!(preservation, PreservationProof::IntCountdownLiteralZero),
        "preservation proof must be IntCountdownLiteralZero, got {:?}",
        preservation
    );
    assert!(
        matches!(decrease, DecreaseProof::NatAbsCountdown),
        "decrease proof must be NatAbsCountdown, got {:?}",
        decrease
    );
    assert_eq!(
        precondition.len(),
        legacy_precondition.len(),
        "precondition arity mismatch with legacy plan"
    );
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    for (lifted, legacy) in precondition.iter().zip(legacy_precondition.iter()) {
        assert_eq!(
            spanned_repr(&lifted.expr),
            spanned_repr_ast(&inputs, legacy, None),
            "precondition clause AST diverges from legacy"
        );
        assert_eq!(
            lifted.free_vars.len(),
            1,
            "precondition clauses bind exactly the countdown param"
        );
        let (var_name, var_ty) = &lifted.free_vars[0];
        assert_eq!(var_name, countdown_param);
        assert!(matches!(var_ty, QuantifierType::Plain(t) if t == "Int"));
    }

    // Body decomposition matches the legacy plan's extracted arms.
    let RecursionPlan::IntCountdownGuarded {
        base_arm_literal: legacy_lit,
        base_arm_body: legacy_base,
        wildcard_arm_body: legacy_wild,
        ..
    } = plans.get("fibTR").unwrap()
    else {
        unreachable!();
    };
    assert_eq!(body.base_arm_literal, *legacy_lit);
    assert_eq!(
        spanned_repr(&body.base_arm_body),
        spanned_repr_ast(&inputs, legacy_base, None)
    );
    assert_eq!(
        spanned_repr(&body.wildcard_arm_body),
        spanned_repr_ast(&inputs, legacy_wild, None),
    );
}

#[test]
fn exposed_int_countdown_lowers_to_fuel_contract() {
    // Closed-world rejection: when the entry module's `exposes` list
    // names the countdown fn, external callers may pass negatives,
    // so the classifier picks plain `IntCountdown` (fuel-encoded)
    // over the native-guarded shape. ProofIR's translation: a Fuel
    // contract with NatAbsPlusOne measure on the countdown param.
    // No precondition derivation, no body decomposition — fuel
    // sidesteps both.
    let src = "module Worker\n\
         \x20   intent = \"t\"\n\
         \x20   exposes [exposed_count]\n\
         \n\
         fn exposed_count(n: Int) -> Int\n\
         \x20   match n\n\
         \x20       0 -> 0\n\
         \x20       _ -> exposed_count(n - 1)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    let RecursionPlan::IntCountdown {
        param_index: legacy_idx,
    } = plans
        .get("exposed_count")
        .unwrap_or_else(|| panic!("exposed_count expected as IntCountdown, got: {:?}", plans))
    else {
        panic!(
            "expected legacy IntCountdown, got: {:?}",
            plans.get("exposed_count")
        );
    };

    let contract =
        fn_contract(&ctx, "exposed_count").expect("exposed_count has no FnContract in ProofIR");
    let RecursionContract::Fuel { fuel_metric } = contract
        .recursion
        .as_ref()
        .expect("contract has no recursion")
    else {
        panic!(
            "exposed_count contract must be Fuel, got: {:?}",
            contract.recursion
        );
    };

    let FuelMetric::NatAbsPlusOne { param } = fuel_metric else {
        panic!("fuel metric must be NatAbsPlusOne, got: {:?}", fuel_metric);
    };

    // Sanity: the bound param matches the legacy plan's chosen index.
    let fd = ctx
        .items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) if fd.name == "exposed_count" => Some(fd),
            _ => None,
        })
        .expect("exposed_count FnDef");
    assert_eq!(param, &fd.params[*legacy_idx].0);
}

#[test]
fn int_ascending_lowers_to_bound_fuel_contract() {
    // IntAscending: param climbs toward a bound checked via
    // `match param == BOUND { true -> base; false -> rec(param + k) }`.
    // Fuel formula `(bound - n).natAbs + 1`. ProofIR captures the
    // bound as `Spanned<Expr>` so backends render through their own
    // emitters (literal here, but it can be a non-trivial arith
    // expression).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn climb(n: Int) -> Int\n\
         \x20   match n == 10\n\
         \x20       true -> 0\n\
         \x20       false -> climb(n + 1)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    let RecursionPlan::IntAscending {
        param_index: legacy_idx,
        bound: legacy_bound,
    } = plans
        .get("climb")
        .unwrap_or_else(|| panic!("climb expected as IntAscending, got: {:?}", plans))
    else {
        panic!(
            "expected legacy IntAscending, got: {:?}",
            plans.get("climb")
        );
    };

    let contract = fn_contract(&ctx, "climb").expect("climb has no FnContract");
    let RecursionContract::Fuel { fuel_metric } = contract
        .recursion
        .as_ref()
        .expect("contract has no recursion")
    else {
        panic!("climb contract must be Fuel, got: {:?}", contract.recursion);
    };

    let FuelMetric::BoundMinusParamNatAbsPlusOne { param, bound } = fuel_metric else {
        panic!(
            "fuel metric must be BoundMinusParamNatAbsPlusOne, got: {:?}",
            fuel_metric
        );
    };

    let fd = ctx
        .items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) if fd.name == "climb" => Some(fd),
            _ => None,
        })
        .expect("climb FnDef");
    assert_eq!(param, &fd.params[*legacy_idx].0);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    assert_eq!(
        spanned_repr(bound),
        spanned_repr_ast(&inputs, legacy_bound, None)
    );
}

#[test]
fn list_structural_lowers_to_seq_len_fuel_contract() {
    // ListStructural — `match xs { [] -> base; [x, ..rest] -> rec(rest, ...) }`.
    // ProofIR carries `SeqLenPlusOne { param }` for symmetry with
    // the other fuel metrics; backends that emit structural recursion
    // natively (Lean / Dafny via List induction) read the param name
    // for the termination measure and ignore the +1 part.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn len(xs: List<Int>) -> Int\n\
         \x20   match xs\n\
         \x20       [] -> 0\n\
         \x20       [_, ..rest] -> 1 + len(rest)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    let RecursionPlan::ListStructural {
        param_index: legacy_idx,
        peel,
    } = plans
        .get("len")
        .unwrap_or_else(|| panic!("len expected as ListStructural, got: {:?}", plans))
    else {
        panic!(
            "expected legacy ListStructural, got: {:?}",
            plans.get("len")
        );
    };
    assert_eq!(
        *peel, 1,
        "`len` peels one cell per step — a plan claiming otherwise would let a \
         backend state a decrease the body does not make"
    );

    let contract = fn_contract(&ctx, "len").expect("len has no FnContract");
    let RecursionContract::Fuel { fuel_metric } = contract
        .recursion
        .as_ref()
        .expect("contract has no recursion")
    else {
        panic!("len contract must be Fuel, got: {:?}", contract.recursion);
    };
    let FuelMetric::SeqLenPlusOne { param } = fuel_metric else {
        panic!("fuel metric must be SeqLenPlusOne, got: {:?}", fuel_metric);
    };

    let fd = ctx
        .items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) if fd.name == "len" => Some(fd),
            _ => None,
        })
        .expect("len FnDef");
    assert_eq!(param, &fd.params[*legacy_idx].0);
}

#[test]
fn two_cell_peel_is_list_structural_and_records_its_depth() {
    // `rest` is the tail of `afterFirst`, which is the tail of the parameter
    // `xs` — a tail of a tail. The classifier reaches it through the
    // transitive tail-binder closure and records that the step consumes TWO
    // cells; without the closure the whole fn fell outside the proof subset
    // and exported opaque.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn pairs(xs: List<Int>) -> Int\n\
         \x20   match xs\n\
         \x20       [] -> 0\n\
         \x20       [_, ..afterFirst] -> match afterFirst\n\
         \x20           [] -> 1\n\
         \x20           [_, ..rest] -> 1 + pairs(rest)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, issues) = analyze_plans_in_scope(&inputs, None, true);
    assert!(
        issues.is_empty(),
        "a two-cell peel is structural — it must not be reported outside the \
         proof subset: {issues:?}"
    );
    assert_eq!(
        plans.get("pairs"),
        Some(&RecursionPlan::ListStructural {
            param_index: 0,
            peel: 2,
        }),
        "expected a two-cell ListStructural plan, got: {:?}",
        plans.get("pairs")
    );
}

#[test]
fn sizeof_structural_lowers_to_sizeof_fuel_contract() {
    // SizeOfStructural — recursion on a user ADT. Fuel formula
    // `sizeOf(call_frame) + 1`, classifier-side measure walks the
    // whole frame so the IR variant carries no param binding.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Tree\n\
         \x20   Leaf\n\
         \x20   Node(Tree, Tree)\n\
         \n\
         fn count(t: Tree) -> Int\n\
         \x20   match t\n\
         \x20       Tree.Leaf -> 1\n\
         \x20       Tree.Node(l, r) -> count(l) + count(r)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    assert!(
        matches!(plans.get("count"), Some(RecursionPlan::SizeOfStructural)),
        "count expected as SizeOfStructural, got: {:?}",
        plans.get("count")
    );

    let contract = fn_contract(&ctx, "count").expect("count has no FnContract");
    let RecursionContract::Fuel { fuel_metric } = contract
        .recursion
        .as_ref()
        .expect("contract has no recursion")
    else {
        panic!("count contract must be Fuel, got: {:?}", contract.recursion);
    };
    assert!(
        matches!(fuel_metric, FuelMetric::SizeOfPlusOne),
        "fuel metric must be SizeOfPlusOne, got: {:?}",
        fuel_metric
    );
}

#[test]
fn string_pos_advance_lowers_to_string_pos_fuel_contract() {
    // StringPosAdvance — `(s: String, pos: Int)` pair with `s`
    // preserved and `pos` advancing. Fuel formula `s.length - pos`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn walk(s: String, pos: Int) -> Int\n\
         \x20   match pos < String.len(s)\n\
         \x20       false -> 0\n\
         \x20       true -> walk(s, pos + 1)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    assert!(
        matches!(plans.get("walk"), Some(RecursionPlan::StringPosAdvance)),
        "walk expected as StringPosAdvance, got: {:?}",
        plans.get("walk")
    );

    let contract = fn_contract(&ctx, "walk").expect("walk has no FnContract");
    let RecursionContract::Fuel { fuel_metric } = contract
        .recursion
        .as_ref()
        .expect("contract has no recursion")
    else {
        panic!("walk contract must be Fuel, got: {:?}", contract.recursion);
    };
    let FuelMetric::StringLenMinusPos {
        string_param,
        pos_param,
    } = fuel_metric
    else {
        panic!(
            "fuel metric must be StringLenMinusPos, got: {:?}",
            fuel_metric
        );
    };
    let fd = ctx
        .items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) if fd.name == "walk" => Some(fd),
            _ => None,
        })
        .expect("walk FnDef");
    assert_eq!(string_param, &fd.params[0].0);
    assert_eq!(pos_param, &fd.params[1].0);
}

#[test]
fn mutual_int_countdown_lowers_to_lex_fuel_contract() {
    // Canonical mutual-Int-countdown shape: even/odd SCC. Every
    // member's recursive call to its peer decrements the shared
    // first-Int param. ProofIR lowers each member to a Lex fuel
    // metric carrying the first-param name + rank 0 (no inter-
    // member ranking).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn even(n: Int) -> Bool\n\
         \x20   match n\n\
         \x20       0 -> true\n\
         \x20       _ -> odd(n - 1)\n\
         \n\
         fn odd(n: Int) -> Bool\n\
         \x20   match n\n\
         \x20       0 -> false\n\
         \x20       _ -> even(n - 1)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    for fn_name in ["even", "odd"] {
        assert!(
            matches!(plans.get(fn_name), Some(RecursionPlan::MutualIntCountdown)),
            "{} expected as MutualIntCountdown, got: {:?}",
            fn_name,
            plans.get(fn_name),
        );

        let contract =
            fn_contract(&ctx, fn_name).unwrap_or_else(|| panic!("{} has no FnContract", fn_name));
        let RecursionContract::Fuel { fuel_metric } = contract
            .recursion
            .as_ref()
            .expect("contract has no recursion")
        else {
            panic!(
                "{} contract must be Fuel, got: {:?}",
                fn_name, contract.recursion
            );
        };
        let FuelMetric::Lex { params, rank } = fuel_metric else {
            panic!("{} metric must be Lex, got: {:?}", fn_name, fuel_metric);
        };
        assert_eq!(params, &vec!["n".to_string()]);
        assert_eq!(*rank, 0);
    }
}

#[test]
fn linear_recurrence_lowers_to_dedicated_contract() {
    // Canonical LinearRecurrence2 shape: f(n) = f(n-1) + f(n-2) with
    // literal 0/1 base + `n < 0` guard. ProofIR lowers to a dedicated
    // RecursionContract::LinearRecurrence2 marker — pair-state Nat
    // worker emission, no fuel.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fib(n: Int) -> Int\n\
         \x20   match n < 0\n\
         \x20       true -> 0\n\
         \x20       false -> match n\n\
         \x20           0 -> 0\n\
         \x20           1 -> 1\n\
         \x20           _ -> fib(n - 1) + fib(n - 2)\n";
    let ctx = build_ctx(src);
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let (plans, _) = analyze_plans_in_scope(&inputs, None, true);

    assert!(
        matches!(plans.get("fib"), Some(RecursionPlan::LinearRecurrence2)),
        "fib expected as LinearRecurrence2, got: {:?}",
        plans.get("fib"),
    );

    let contract = fn_contract(&ctx, "fib").expect("fib has no FnContract");
    assert!(
        matches!(
            contract.recursion,
            Some(RecursionContract::LinearRecurrence2)
        ),
        "fib contract must be LinearRecurrence2, got: {:?}",
        contract.recursion,
    );
}

#[test]
fn law_lower_populates_theorems_from_verify_law_blocks() {
    // `verify add law commutative ... add(a, b) => add(b, a)` →
    // ProofIR.law_theorems gets an entry with quantifiers extracted
    // from `given a/b: Int`, premises empty (no `when` clause), and
    // claim_lhs / claim_rhs pointing at the source AST nodes.
    // Strategy stays `BackendDispatch` until subsequent Steps
    // migrate concrete strategies (rfl, induction, …) into the
    // lowerer.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn add(a: Int, b: Int) -> Int\n\
         \x20   a + b\n\
         \n\
         verify add law commutative\n\
         \x20   given a: Int = -2..2\n\
         \x20   given b: Int = -2..2\n\
         \x20   add(a, b) => add(b, a)\n";
    let ctx = build_ctx(src);

    let theorem = law_theorem(&ctx, "add", "commutative")
        .expect("add::commutative law theorem missing from ProofIR");

    assert_eq!(theorem.quantifiers.len(), 2, "expected 2 quantifiers");
    assert_eq!(theorem.quantifiers[0].name, "a");
    assert_eq!(theorem.quantifiers[1].name, "b");
    assert!(matches!(
        &theorem.quantifiers[0].binder_type,
        QuantifierType::Plain(t) if t == "Int"
    ));
    assert!(
        theorem.premises.is_empty(),
        "no `when` clause → no premises, got: {:?}",
        theorem.premises
    );
    // `add(a, b) => add(b, a)` — Step 25 pins
    // `Commutative { op: Add }` for this shape (2-arg Int
    // wrapper around `BinOp::Add`).
    assert!(
        matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::Commutative {
                op: aver::ast::BinOp::Add
            }
        ),
        "commutative on Add-wrapper must pin WrapperCommutative, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn reflexive_law_pinned_when_lhs_equals_rhs() {
    // `x => x` is the canonical Reflexive shape. After Step 24
    // proof_lower pins `ProofStrategy::Reflexive`, and Lean's
    // law_auto reads from there instead of running the syntactic
    // equality check ad-hoc.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn id(a: Int) -> Int\n\
         \x20   a\n\
         \n\
         verify id law reflexive\n\
         \x20   given x: Int = -2..2\n\
         \x20   x => x\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "id", "reflexive")
        .expect("id::reflexive law theorem missing from ProofIR");
    assert!(
        matches!(theorem.strategy, aver::ir::ProofStrategy::Reflexive),
        "x => x must pin Reflexive, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn wrapper_associative_pinned_on_three_int_givens_assoc_shape() {
    // `add(add(a,b),c) => add(a,add(b,c))` over `fn add(a,b) -> a+b`
    // — Step 25 pins `Associative { op: Add }`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn add(a: Int, b: Int) -> Int\n\
         \x20   a + b\n\
         \n\
         verify add law associative\n\
         \x20   given a: Int = -1..1\n\
         \x20   given b: Int = -1..1\n\
         \x20   given c: Int = -1..1\n\
         \x20   add(add(a, b), c) => add(a, add(b, c))\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "add", "associative").expect("add::associative law theorem missing");
    assert!(
        matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::Associative {
                op: aver::ast::BinOp::Add
            }
        ),
        "expected WrapperAssociative, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn wrapper_identity_pinned_on_add_with_zero_rhs() {
    // `add(a, 0) => a` over `fn add(a,b) -> a+b` — pins
    // `IdentityElement { op: Add }`. The identity literal is
    // implicit (`0` for Add; `1` for Mul).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn add(a: Int, b: Int) -> Int\n\
         \x20   a + b\n\
         \n\
         verify add law identityZero\n\
         \x20   given a: Int = -3..3\n\
         \x20   add(a, 0) => a\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "add", "identityZero").expect("add::identityZero law theorem missing");
    assert!(
        matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::IdentityElement {
                op: aver::ast::BinOp::Add
            }
        ),
        "expected WrapperIdentity, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn wrapper_sub_right_identity_pinned_on_sub_with_zero_rhs() {
    // `sub(a, 0) => a` over `fn sub(a, b) -> a - b` — Step 26 pins
    // `IdentityElement { op: aver::ast::BinOp::Sub }`. Sub-specific because subtraction's
    // identity is one-sided.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn sub(a: Int, b: Int) -> Int\n\
         \x20   a - b\n\
         \n\
         verify sub law rightIdentity\n\
         \x20   given a: Int = -3..3\n\
         \x20   sub(a, 0) => a\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "sub", "rightIdentity").expect("sub::rightIdentity law theorem missing");
    assert!(
        matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::IdentityElement {
                op: aver::ast::BinOp::Sub
            }
        ),
        "expected IdentityElement {{ op: Sub }}, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn wrapper_sub_anti_commutative_pinned_with_neg_direction() {
    // `sub(a, b) => -sub(b, a)` over `fn sub(a, b) -> a - b` —
    // `AntiCommutative { op: aver::ast::BinOp::Sub, neg_on_rhs: true }`. The IR's
    // direction flag drives `.symm` selection on the Lean side.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn sub(a: Int, b: Int) -> Int\n\
         \x20   a - b\n\
         \n\
         verify sub law antiCommutative\n\
         \x20   given a: Int = -2..2\n\
         \x20   given b: Int = -2..2\n\
         \x20   sub(a, b) => -sub(b, a)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "sub", "antiCommutative")
        .expect("sub::antiCommutative law theorem missing");
    assert!(
        matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::AntiCommutative {
                op: aver::ast::BinOp::Sub,
                neg_on_rhs: true
            }
        ),
        "expected WrapperSubAntiCommutative {{ neg_on_rhs: true }}, got: {:?}",
        theorem.strategy,
    );
}

#[test]
fn wrapper_unary_equivalence_pinned_with_inner_fn_name() {
    // `addOne(a) => add(a, 1)` over `fn addOne(a) -> a + 1` and
    // `fn add(a, b) -> a + b` — Step 27 pins
    // `UnaryEqualsBinary { inner_fn: "add" }`. The inner fn
    // name lives in the IR so the backend renders
    // `simp [addOne, add]` without rescanning.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn add(a: Int, b: Int) -> Int\n\
         \x20   a + b\n\
         \n\
         fn addOne(a: Int) -> Int\n\
         \x20   a + 1\n\
         \n\
         verify addOne law identityViaAdd\n\
         \x20   given a: Int = -2..2\n\
         \x20   addOne(a) => add(a, 1)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "addOne", "identityViaAdd")
        .expect("addOne::identityViaAdd law theorem missing");
    let aver::ir::ProofStrategy::UnaryEqualsBinary { ref inner_fn } = theorem.strategy else {
        panic!("expected UnaryEqualsBinary, got: {:?}", theorem.strategy);
    };
    assert_eq!(inner_fn, "add");
}

#[test]
fn simp_omega_unfold_pinned_on_sub_anti_comm_via_zero() {
    // `sub(a, b) => 0 - sub(b, a)` doesn't fit
    // WrapperSubAntiCommutative (that requires `Neg(call)` form);
    // falls through to LinearArithmetic. The IR captures the
    // unfold list (just `sub` — non-recursive) plus
    // `wrapper_return: false` (sub returns Int).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn sub(a: Int, b: Int) -> Int\n\
         \x20   a - b\n\
         \n\
         verify sub law antiCommutative\n\
         \x20   given a: Int = -2..2\n\
         \x20   given b: Int = -2..2\n\
         \x20   sub(a, b) => 0 - sub(b, a)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "sub", "antiCommutative")
        .expect("sub::antiCommutative law theorem missing");
    let aver::ir::ProofStrategy::LinearArithmetic {
        ref unfold_fns,
        wrapper_return,
        ref smart_guard,
        lifted,
    } = theorem.strategy
    else {
        panic!("expected LinearArithmetic, got: {:?}", theorem.strategy);
    };
    assert!(unfold_fns.contains(&"sub".to_string()));
    assert!(!wrapper_return, "sub returns Int, not a wrapper");
    assert!(!lifted, "no refinement lift in plain Int law");
    assert!(smart_guard.is_none(), "no refinement in chain");
}

#[test]
fn linear_arithmetic_pinned_with_lifted_for_refinement_law() {
    // Refinement-lifted law: givens are Int but used as
    // `Natural(value = a)` carriers in the law body. The outer fn
    // takes Natural params; the legacy backend bypassed the
    // strategy chain via `refinement_auto_proof` in toplevel.rs.
    // Step 30 routes these through `LinearArithmetic { lifted:
    // true }` — backend skips by_cases (Subtype carries the
    // invariant) and emits the unfold + simp tactic directly.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         record Natural\n\
         \x20   value: Int\n\
         \n\
         fn fromInt(n: Int) -> Result<Natural, String>\n\
         \x20   match n >= 0\n\
         \x20       true -> Result.Ok(Natural(value = n))\n\
         \x20       false -> Result.Err(\"negative\")\n\
         \n\
         fn add(a: Natural, b: Natural) -> Result<Natural, String>\n\
         \x20   fromInt(a.value + b.value)\n\
         \n\
         verify add law commutative\n\
         \x20   given a: Int = [0, 1, 7]\n\
         \x20   given b: Int = [0, 1, 7]\n\
         \x20   when a >= 0\n\
         \x20   when b >= 0\n\
         \x20   add(Natural(value = a), Natural(value = b)) => add(Natural(value = b), Natural(value = a))\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "add", "commutative").expect("add::commutative law theorem missing");
    let aver::ir::ProofStrategy::LinearArithmetic {
        wrapper_return,
        lifted,
        ..
    } = theorem.strategy
    else {
        panic!(
            "expected LinearArithmetic for refinement-lifted law, got: {:?}",
            theorem.strategy
        );
    };
    assert!(wrapper_return, "add returns Result, wrapper");
    assert!(lifted, "givens used as Natural carriers — lifted=true");
}

#[test]
fn induction_pinned_on_recursive_adt_given() {
    // Sum type `Tree` with direct recursion via `Node(Tree, Tree)`.
    // A law over `Tree` gets pinned `Induction { param }` — the
    // case-split shape the backend's `emit_structural_induction_
    // law` consumes. Reflexive would also fit `f(t) = f(t)` but
    // induction's chain priority wins (matches legacy behaviour).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Tree\n\
         \x20   Leaf\n\
         \x20   Node(Tree, Tree)\n\
         \n\
         fn id(t: Tree) -> Tree\n\
         \x20   t\n\
         \n\
         verify id law identity\n\
         \x20   given t: Tree = [Tree.Leaf]\n\
         \x20   id(t) => t\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "id", "identity").expect("id::identity law theorem missing");
    let aver::ir::ProofStrategy::Induction { ref param } = theorem.strategy else {
        panic!(
            "expected Induction {{ param }}, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(param, "t");
}

#[test]
fn library_axiom_pinned_on_map_has_set_self() {
    // `Map.has(Map.set(m, k, v), k) => true` — canonical
    // has-after-set axiom. Step 32 pins
    // `LibraryAxiom { axiom: "Map.has_set_self", args: [m, k, v] }`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn touch(m: Map<String, Int>, k: String, v: Int) -> Map<String, Int>\n\
         \x20   Map.set(m, k, v)\n\
         \n\
         verify touch law hasAfterSet\n\
         \x20   given m: Map<String, Int> = [{}]\n\
         \x20   given k: String = [\"x\"]\n\
         \x20   given v: Int = [1]\n\
         \x20   Map.has(Map.set(m, k, v), k) => true\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "touch", "hasAfterSet").expect("touch::hasAfterSet law theorem missing");
    let aver::ir::ProofStrategy::LibraryAxiom {
        ref axiom,
        ref args,
    } = theorem.strategy
    else {
        panic!("expected LibraryAxiom, got: {:?}", theorem.strategy);
    };
    assert_eq!(axiom, "Map.has_set_self");
    assert_eq!(args.len(), 3, "args should be [m, k, v]");
}

#[test]
fn map_update_postcondition_pinned_has_after() {
    // `Map.has(incCount(counts, word), word) => true` — `incCount`'s
    // body inspects `Map.get(counts, word)` and `Map.set`s on every
    // arm. Step 33 pins `MapUpdatePostcondition { kind: HasAfter,
    // outer_fn: "incCount", … }`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn incCount(counts: Map<String, Int>, word: String) -> Map<String, Int>\n\
         \x20   current = Map.get(counts, word)\n\
         \x20   match current\n\
         \x20       Option.Some(n) -> Map.set(counts, word, n + 1)\n\
         \x20       Option.None -> Map.set(counts, word, 1)\n\
         \n\
         verify incCount law keyPresent\n\
         \x20   given counts: Map<String, Int> = [{}]\n\
         \x20   given word: String = [\"a\"]\n\
         \x20   Map.has(incCount(counts, word), word) => true\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "incCount", "keyPresent")
        .expect("incCount::keyPresent law theorem missing");
    let aver::ir::ProofStrategy::MapUpdatePostcondition {
        ref outer_fn,
        kind,
        ref extra_unfolds,
        ..
    } = theorem.strategy
    else {
        panic!(
            "expected MapUpdatePostcondition, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(outer_fn, "incCount");
    assert_eq!(kind, aver::ir::MapUpdatePostconditionKind::HasAfter);
    assert!(
        extra_unfolds.is_empty(),
        "HasAfter shouldn't carry helper unfolds, got: {extra_unfolds:?}"
    );
}

#[test]
fn map_update_postcondition_pinned_get_after_with_helper_unfolds() {
    // `Map.get(incCount(counts, "a"), "a") => Option.Some(addOne(...))`
    // — same `incCount` body shape, GetAfter variant pulls helper
    // fns (`addOne`) into the IR's `extra_unfolds` set.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn addOne(n: Int) -> Int\n\
         \x20   n + 1\n\
         \n\
         fn incCount(counts: Map<String, Int>, word: String) -> Map<String, Int>\n\
         \x20   current = Map.get(counts, word)\n\
         \x20   match current\n\
         \x20       Option.Some(n) -> Map.set(counts, word, n + 1)\n\
         \x20       Option.None -> Map.set(counts, word, 1)\n\
         \n\
         verify incCount law existingKeyIncrements\n\
         \x20   given counts: Map<String, Int> = [{\"a\" => 1}]\n\
         \x20   Map.get(incCount(counts, \"a\"), \"a\") => Option.Some(addOne(Option.withDefault(Map.get(counts, \"a\"), 0)))\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "incCount", "existingKeyIncrements")
        .expect("incCount::existingKeyIncrements law theorem missing");
    let aver::ir::ProofStrategy::MapUpdatePostcondition {
        ref outer_fn,
        kind,
        ref extra_unfolds,
        ..
    } = theorem.strategy
    else {
        panic!(
            "expected MapUpdatePostcondition, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(outer_fn, "incCount");
    assert_eq!(kind, aver::ir::MapUpdatePostconditionKind::GetAfter);
    assert_eq!(
        extra_unfolds,
        &vec!["addOne".to_string()],
        "GetAfter should carry the helper-fn unfold set (outer fn excluded)"
    );
}

#[test]
fn map_key_tracked_increment_pinned_on_defaulted_get_plus_one() {
    // `Option.withDefault(Map.get(incCount(m, k), k), 0) ==
    // Option.withDefault(Map.get(m, k), 0) + 1` — the canonical
    // tracked-counter increment law. Outer fn body is the specific
    // `Some(n) -> n + 1 / None -> 1` template. Step 34 pins
    // `MapKeyTrackedIncrement { outer_fn: "incCount", … }`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn incCount(counts: Map<String, Int>, word: String) -> Map<String, Int>\n\
         \x20   current = Map.get(counts, word)\n\
         \x20   match current\n\
         \x20       Option.Some(n) -> Map.set(counts, word, n + 1)\n\
         \x20       Option.None -> Map.set(counts, word, 1)\n\
         \n\
         verify incCount law trackedCountStepsByOne\n\
         \x20   given counts: Map<String, Int> = [{}]\n\
         \x20   given word: String = [\"a\"]\n\
         \x20   Option.withDefault(Map.get(incCount(counts, word), word), 0) => Option.withDefault(Map.get(counts, word), 0) + 1\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "incCount", "trackedCountStepsByOne")
        .expect("incCount::trackedCountStepsByOne law theorem missing");
    let aver::ir::ProofStrategy::MapKeyTrackedIncrement { ref outer_fn, .. } = theorem.strategy
    else {
        panic!(
            "expected MapKeyTrackedIncrement, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(outer_fn, "incCount");
}

#[test]
fn spec_equivalence_pinned_on_identical_body_impl_spec_pair() {
    // `verify absVal law absValSpec` with `absVal(x) => absValSpec(x)`
    // and identical bodies in both fns. Step 37 pins
    // `SpecEquivalence { extra_unfolds: [absVal, absValSpec] }`.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn absVal(x: Int) -> Int\n\
         \x20   match x < 0\n\
         \x20       true -> 0 - x\n\
         \x20       false -> x\n\
         \n\
         fn absValSpec(x: Int) -> Int\n\
         \x20   match x < 0\n\
         \x20       true -> 0 - x\n\
         \x20       false -> x\n\
         \n\
         verify absVal law absValSpec\n\
         \x20   given x: Int = [0]\n\
         \x20   absVal(x) => absValSpec(x)\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "absVal", "absValSpec").expect("absVal::absValSpec law theorem missing");
    let aver::ir::ProofStrategy::SpecEquivalence { ref extra_unfolds } = theorem.strategy else {
        panic!("expected SpecEquivalence, got: {:?}", theorem.strategy);
    };
    assert_eq!(
        extra_unfolds,
        &vec!["absVal".to_string(), "absValSpec".to_string()],
        "extra_unfolds should be the impl + spec pair (sorted)"
    );
}

#[test]
fn effectful_spec_equivalence_pinned_post_oracle_lift() {
    // Effectful impl + spec via Random.int oracle. Source-level law
    // shape `pickPair() => pairSpec(BranchPath.Root, rnd)` has
    // mismatched arg counts (impl 0, spec 2) — non-canonical.
    // Step 38's Oracle Lift in the detector injects
    // `(BranchPath.Root, rnd)` into the impl call, both sides
    // become `pickPair(BranchPath.Root, rnd) == pairSpec(...)` with
    // identical args, and the canonical-shape match succeeds.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn counterStub(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   n\n\
         \n\
         fn pairSpec(path: BranchPath, rnd: Fn(BranchPath, Int, Int, Int) -> Int) -> Tuple<Int, Int>\n\
         \x20   (rnd(BranchPath.child(path, 0), 0, 1, 6), rnd(BranchPath.child(path, 1), 0, 7, 12))\n\
         \n\
         fn pickPair() -> Tuple<Int, Int>\n\
         \x20   ! [Random.int]\n\
         \x20   (Random.int(1, 6), Random.int(7, 12))!\n\
         \n\
         verify pickPair law branchPathLaw\n\
         \x20   given rnd: Random.int = [counterStub]\n\
         \x20   pickPair() => pairSpec(BranchPath.Root, rnd)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "pickPair", "branchPathLaw")
        .expect("pickPair::branchPathLaw law theorem missing");
    let aver::ir::ProofStrategy::EffectfulSpecEquivalence {
        ref impl_fn,
        ref spec_fn,
    } = theorem.strategy
    else {
        panic!(
            "expected EffectfulSpecEquivalence, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(impl_fn, "pickPair");
    assert_eq!(spec_fn, "pairSpec");
}

#[test]
fn simp_normalized_spec_equivalence_pinned_on_arithmetic_identity_gap() {
    // `square(x) = x * x` vs `squareSpec(x) = x * x + 0`. Bodies
    // differ syntactically; after arg substitution + dropping the
    // redundant `+ 0` they normalize to the same `x * x`. Step 39
    // pins `SpecEquivalenceSimpNormalized` — distinct from Step 37's
    // strict `SpecEquivalence` (which requires identical raw bodies).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn square(x: Int) -> Int\n\
         \x20   x * x\n\
         \n\
         fn squareSpec(x: Int) -> Int\n\
         \x20   x * x + 0\n\
         \n\
         verify square law squareSpec\n\
         \x20   given x: Int = [0]\n\
         \x20   square(x) => squareSpec(x)\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "square", "squareSpec").expect("square::squareSpec law theorem missing");
    let aver::ir::ProofStrategy::SpecEquivalenceSimpNormalized { ref extra_unfolds } =
        theorem.strategy
    else {
        panic!(
            "expected SpecEquivalenceSimpNormalized, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(
        extra_unfolds,
        &vec!["square".to_string(), "squareSpec".to_string()],
        "extra_unfolds should be the impl + spec pair (sorted)"
    );
}

#[test]
fn linear_int_spec_equivalence_pinned_on_commutative_addition() {
    // `addOne(n) = n + 1` vs `addOneSpec(n) = 1 + n`. Bodies differ
    // (operand order), substitute n into both → `n + 1` and `1 + n`
    // — pure linear-int arithmetic over the given `n`. Step 40 pins
    // `LinearIntSpecEquivalence { unfolded_impl, unfolded_spec }`.
    // Distinct from `SpecEquivalenceSimpNormalized` because the
    // `simplify_identity_expr` pass doesn't rewrite `1 + n` to
    // `n + 1` (no commutativity normalisation), so the body-match
    // check there fails and falls through to this detector.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn addOne(n: Int) -> Int\n\
         \x20   n + 1\n\
         \n\
         fn addOneSpec(n: Int) -> Int\n\
         \x20   1 + n\n\
         \n\
         verify addOne law addOneSpec\n\
         \x20   given n: Int = [0]\n\
         \x20   addOne(n) => addOneSpec(n)\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "addOne", "addOneSpec").expect("addOne::addOneSpec law theorem missing");
    let aver::ir::ProofStrategy::LinearIntSpecEquivalence {
        ref unfolded_impl,
        ref unfolded_spec,
    } = theorem.strategy
    else {
        panic!(
            "expected LinearIntSpecEquivalence, got: {:?}",
            theorem.strategy
        );
    };
    // Asserting full Spanned AST equality is brittle (carries
    // OnceLock type cells); just check the shape stripped to Debug.
    let impl_repr = format!("{:?}", unfolded_impl.node);
    let spec_repr = format!("{:?}", unfolded_spec.node);
    assert!(
        impl_repr.contains("Ident(\"n\")") && impl_repr.contains("Int(1)"),
        "unfolded_impl should reference n and literal 1, got: {impl_repr}"
    );
    assert!(
        spec_repr.contains("Ident(\"n\")") && spec_repr.contains("Int(1)"),
        "unfolded_spec should reference n and literal 1, got: {spec_repr}"
    );
}

#[test]
fn enum_constant_fold_pinned_for_constructor_pinned_ground_law() {
    // A non-recursive fn with a non-Int (enum) param, the law pins
    // that param to a constructor literal and leaves the Int given
    // quantified-but-unused — the canonical EnumConstantFold shape
    // (mirrors `centerBonus.emptyNeutral` in
    // `examples/games/checkers/ai.av`). No earlier detector accepts
    // it: LinearArithmetic rejects the non-Int param, Induction needs
    // a recursive-ADT given.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Color\n\
         \x20   Black\n\
         \x20   White\n\
         \n\
         fn score(c: Color, x: Int) -> Int\n\
         \x20   match c\n\
         \x20       Color.Black -> 0\n\
         \x20       Color.White -> 1\n\
         \n\
         verify score law blackIsZero\n\
         \x20   given x: Int = 0..3\n\
         \x20   score(Color.Black, x) => 0\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "score", "blackIsZero")
        .expect("score::blackIsZero law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::EnumConstantFold { ref unfold_fns } = theorem.strategy else {
        panic!(
            "constructor-pinned ground law must pin EnumConstantFold, got: {:?}",
            theorem.strategy
        );
    };
    assert!(
        unfold_fns.iter().any(|n| n == "score"),
        "unfold list must include the verified fn, got: {unfold_fns:?}"
    );
}

#[test]
fn enum_constant_fold_not_pinned_when_adt_param_unpinned() {
    // CONSERVATIVE guard: the enum param is itself a `given` (a free
    // quantified variable), NOT pinned to a constructor. The
    // `split`/`rfl`/`decide` cascade can't discharge the free enum,
    // so the detector must decline and fall through to
    // BackendDispatch — no false universal claim.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Color\n\
         \x20   Black\n\
         \x20   White\n\
         \n\
         fn score(c: Color, x: Int) -> Int\n\
         \x20   match c\n\
         \x20       Color.Black -> 0\n\
         \x20       Color.White -> 0\n\
         \n\
         verify score law alwaysZero\n\
         \x20   given c: Color = [Color.Black]\n\
         \x20   given x: Int = 0..3\n\
         \x20   score(c, x) => 0\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "score", "alwaysZero")
        .expect("score::alwaysZero law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::EnumConstantFold { .. }
        ),
        "law with an unpinned enum given must NOT pin EnumConstantFold, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn finite_domain_cases_pinned_for_bool_given_law() {
    // A law whose only given is `Bool` — closed two-value domain, so
    // exhaustive `cases` enumeration yields ground goals regardless of
    // the verified fn's shape. The canonical real-corpus shape is
    // `examples/data/json.av` `parseLiteral.boolRoundtrip` (closes
    // genuinely with `intro b; cases b <;> rfl`). The fn here returns
    // a String (EnumConstantFold's scalar-return gate rejects it) and
    // the given is a free quantified variable (its literal-pinning
    // gate rejects it too) — only FiniteDomainCases can own this law.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn tag(b: Bool) -> String\n\
         \x20   match b\n\
         \x20       true -> \"yes\"\n\
         \x20       false -> \"no\"\n\
         \n\
         verify tag law nonEmpty\n\
         \x20   given b: Bool = [true, false]\n\
         \x20   tag(b) == \"\" => false\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "tag", "nonEmpty")
        .expect("tag::nonEmpty law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::FiniteDomainCases { ref givens } = theorem.strategy else {
        panic!(
            "Bool-given law must pin FiniteDomainCases, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(
        givens,
        &vec!["b".to_string()],
        "cases targets must be the given names in intro order"
    );
}

#[test]
fn finite_domain_cases_pinned_for_fieldless_enum_given_law() {
    // A law quantified over a user-declared all-fieldless enum (3
    // ctors → domain size 3 ≤ 16). The verified fn is RECURSIVE —
    // FiniteDomainCases deliberately has no recursion gate (closed
    // enumeration computes through fuel wrappers), which is exactly
    // what distinguishes it from EnumConstantFold.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Color\n\
         \x20   Red\n\
         \x20   Green\n\
         \x20   Blue\n\
         \n\
         fn spin(c: Color, n: Int) -> Int\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> spin(c, n - 1)\n\
         \n\
         verify spin law drains\n\
         \x20   given c: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   spin(c, 2) => 0\n";
    let ctx = build_ctx(src);
    let theorem =
        law_theorem(&ctx, "spin", "drains").expect("spin::drains law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::FiniteDomainCases { ref givens } = theorem.strategy else {
        panic!(
            "fieldless-enum-given law must pin FiniteDomainCases, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(givens, &vec!["c".to_string()]);
}

#[test]
fn finite_domain_cases_not_pinned_for_int_given() {
    // Int is an OPEN domain — `cases` can't enumerate it. The detector
    // must decline even though another given is Bool: EVERY given must
    // be finitely enumerable for the cascade to yield closed goals.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn pick(b: Bool, x: Int) -> String\n\
         \x20   match b\n\
         \x20       true -> \"yes\"\n\
         \x20       false -> \"no\"\n\
         \n\
         verify pick law nonEmpty\n\
         \x20   given b: Bool = [true, false]\n\
         \x20   given x: Int = [0, 1]\n\
         \x20   pick(b, x) == \"\" => false\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "pick", "nonEmpty")
        .expect("pick::nonEmpty law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::FiniteDomainCases { .. }
        ),
        "law with an Int given must NOT pin FiniteDomainCases, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn finite_domain_cases_not_pinned_for_when_law() {
    // `when` premises are out of scope: the cascade has no premise
    // handling (the hypothesis would block the per-leaf `rfl`).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn tag(b: Bool) -> String\n\
         \x20   match b\n\
         \x20       true -> \"yes\"\n\
         \x20       false -> \"no\"\n\
         \n\
         verify tag law yesWhenTrue\n\
         \x20   given b: Bool = [true, false]\n\
         \x20   when b\n\
         \x20   tag(b) => \"yes\"\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "tag", "yesWhenTrue")
        .expect("tag::yesWhenTrue law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::FiniteDomainCases { .. }
        ),
        "when-law must NOT pin FiniteDomainCases, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn finite_domain_cases_not_pinned_for_payload_enum_given() {
    // An enum with a payload-carrying ctor is NOT a closed finite
    // domain — `cases` on it introduces a fresh free variable
    // (`Cell.Piece v`) the per-leaf `rfl`/`decide` cascade can't
    // compute out. The detector must decline. (The law's lhs/rhs are
    // deliberately NOT syntactically equal so `Reflexive` can't pin
    // first and mask the payload gate under test.)
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Cell\n\
         \x20   Empty\n\
         \x20   Piece(Int)\n\
         \n\
         fn isEmpty(c: Cell) -> Bool\n\
         \x20   match c\n\
         \x20       Cell.Empty -> true\n\
         \x20       Cell.Piece(_) -> false\n\
         \n\
         verify isEmpty law selfConsistent\n\
         \x20   given c: Cell = [Cell.Empty, Cell.Piece(1)]\n\
         \x20   isEmpty(c) == isEmpty(c) => true\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "isEmpty", "selfConsistent")
        .expect("isEmpty::selfConsistent law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::FiniteDomainCases { .. }
        ),
        "payload-enum given must NOT pin FiniteDomainCases, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn finite_domain_cases_not_pinned_when_domain_product_exceeds_16() {
    // Domain-size budget: 3 Color givens (3^3 = 27 > 16) would emit a
    // 27-leaf cascade — past the deliberate cap. The detector must
    // decline; 2 givens of the same enum (3^2 = 9 ≤ 16) still fire
    // (asserted as the in-budget control below).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Color\n\
         \x20   Red\n\
         \x20   Green\n\
         \x20   Blue\n\
         \n\
         fn same(a: Color, b: Color, c: Color) -> Bool\n\
         \x20   match a\n\
         \x20       Color.Red -> true\n\
         \x20       Color.Green -> true\n\
         \x20       Color.Blue -> true\n\
         \n\
         verify same law alwaysTrue\n\
         \x20   given a: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   given b: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   given c: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   same(a, b, c) => true\n\
         \n\
         fn pair(a: Color, b: Color) -> Bool\n\
         \x20   match a\n\
         \x20       Color.Red -> true\n\
         \x20       Color.Green -> true\n\
         \x20       Color.Blue -> true\n\
         \n\
         verify pair law alwaysTrue\n\
         \x20   given a: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   given b: Color = [Color.Red, Color.Green, Color.Blue]\n\
         \x20   pair(a, b) => true\n";
    let ctx = build_ctx(src);
    let over = law_theorem(&ctx, "same", "alwaysTrue")
        .expect("same::alwaysTrue law theorem missing from ProofIR");
    assert!(
        !matches!(
            over.strategy,
            aver::ir::ProofStrategy::FiniteDomainCases { .. }
        ),
        "domain product 27 > 16 must NOT pin FiniteDomainCases, got: {:?}",
        over.strategy
    );
    let within = law_theorem(&ctx, "pair", "alwaysTrue")
        .expect("pair::alwaysTrue law theorem missing from ProofIR");
    assert!(
        matches!(
            within.strategy,
            aver::ir::ProofStrategy::FiniteDomainCases { .. }
        ),
        "domain product 9 ≤ 16 control must pin FiniteDomainCases, got: {:?}",
        within.strategy
    );
}

#[test]
fn simp_over_prelude_lemmas_pinned_for_nonrecursive_builtin_roundtrip_law() {
    // A builtin-roundtrip law over a non-recursive String fn — no
    // earlier detector owns it (LinearArithmetic rejects String
    // params, EnumConstantFold needs a constructor-pinned ADT param
    // and a scalar return, FiniteDomainCases needs a closed-domain
    // given), so it used to fall to BackendDispatch + bare `sorry`.
    // The canonical real-corpus shape is `examples/data/json.av`
    // `finishString.plainSegmentRoundtrip`. The pin must carry the
    // subject-first unfold list and the registry keys (note the
    // synthetic `String.concat` marker for the string `+`).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn keepPrefix(s: String, n: Int) -> String\n\
         \x20   String.slice(s, 0, n)\n\
         \n\
         verify keepPrefix law appendRoundtrip\n\
         \x20   given s: String = [\"\", \"ab\"]\n\
         \x20   keepPrefix(s + \"!\", String.len(s)) => s\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "keepPrefix", "appendRoundtrip")
        .expect("keepPrefix::appendRoundtrip law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::SimpOverPreludeLemmas {
        ref unfold_fns,
        ref fuel_fns,
        ref builtins,
    } = theorem.strategy
    else {
        panic!(
            "builtin-roundtrip law must pin SimpOverPreludeLemmas, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(unfold_fns, &vec!["keepPrefix".to_string()]);
    assert!(fuel_fns.is_empty(), "no recursive seed: {:?}", fuel_fns);
    assert_eq!(
        builtins,
        &vec![
            "String.concat".to_string(),
            "String.len".to_string(),
            "String.slice".to_string()
        ],
        "registry keys must cover the cone's builtins + the string-`+` marker"
    );
}

#[test]
fn simp_over_prelude_lemmas_pinned_for_fuel_fn_with_constructor_headed_arg() {
    // The lhs calls a RECURSIVE (fuel-emitted, SizeOfStructural) fn
    // with a constructor-headed arg whose variant has only scalar
    // fields — the ADT measure is constant on `Tree.Leaf(n)` for free
    // `n`, so the fuel computes and the fn lands in `fuel_fns`
    // (json.av's `toString(Json.JsonInt(n))` shape).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         type Tree\n\
         \x20   Leaf(Int)\n\
         \x20   Node(Tree, Tree)\n\
         \n\
         fn render(t: Tree) -> String\n\
         \x20   match t\n\
         \x20       Tree.Leaf(n) -> String.fromInt(n)\n\
         \x20       Tree.Node(l, r) -> render(l) + render(r)\n\
         \n\
         verify render law leafRoundtrip\n\
         \x20   given n: Int = [1, 42]\n\
         \x20   render(Tree.Leaf(n)) => String.fromInt(n)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "render", "leafRoundtrip")
        .expect("render::leafRoundtrip law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::SimpOverPreludeLemmas {
        ref unfold_fns,
        ref fuel_fns,
        ref builtins,
    } = theorem.strategy
    else {
        panic!(
            "ctor-headed fuel-fn law must pin SimpOverPreludeLemmas, got: {:?}",
            theorem.strategy
        );
    };
    assert!(
        unfold_fns.is_empty(),
        "no non-recursive cone: {:?}",
        unfold_fns
    );
    assert_eq!(fuel_fns, &vec!["render".to_string()]);
    assert!(
        builtins.contains(&"String.fromInt".to_string()),
        "fuel-fn BODY builtins must feed the registry keys, got: {:?}",
        builtins
    );
}

#[test]
fn simp_over_prelude_lemmas_not_pinned_for_when_law() {
    // `when` premises are out of scope — the simp set has no premise
    // handling, and the detector must mirror the no-when gates of the
    // sibling fallbacks (EnumConstantFold / FiniteDomainCases).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn keepPrefix(s: String, n: Int) -> String\n\
         \x20   String.slice(s, 0, n)\n\
         \n\
         verify keepPrefix law guardedRoundtrip\n\
         \x20   given s: String = [\"\", \"ab\"]\n\
         \x20   when String.len(s) >= 0\n\
         \x20   keepPrefix(s + \"!\", String.len(s)) => s\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "keepPrefix", "guardedRoundtrip")
        .expect("keepPrefix::guardedRoundtrip law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
        ),
        "when-law must NOT pin SimpOverPreludeLemmas, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn simp_over_prelude_lemmas_not_pinned_for_recursive_seed_with_open_args() {
    // The lhs calls a recursive fn with a FREE Int given — the fuel
    // value (`natAbs n + 1`) stays symbolic, simp can't drive the
    // `__fuel` equations, so the detector must decline (the law keeps
    // today's bare-sorry honesty instead of a doomed simp attempt).
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn countDown(n: Int) -> Int\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> countDown(n - 1)\n\
         \n\
         verify countDown law alwaysZero\n\
         \x20   given n: Int = [0, 3]\n\
         \x20   countDown(n) => 0\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "countDown", "alwaysZero")
        .expect("countDown::alwaysZero law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
        ),
        "open-arg recursive seed must NOT pin SimpOverPreludeLemmas, got: {:?}",
        theorem.strategy
    );
}

/// Canonical decimal-parser source for the `IntDecimalRoundtrip`
/// detector tests — a minimal standalone replica of the
/// `examples/data/json.av` number-parsing family (head-char dispatch,
/// sign path, one fuelized digit scanner, slice + `Int.fromString`
/// leaf, `C(x) -> String.fromInt(x)` serializer arm).
const DECIMAL_ROUNDTRIP_SRC: &str = r#"module DecimalRt
    intent = "decimal int render/parse roundtrip"
    effects []

type Num
    NumInt(Int)

type ParseOut
    Got(Num, Int)
    Bad(String, Int)

fn render(v: Num) -> String
    match v
        Num.NumInt(n) -> String.fromInt(n)

fn isDigit(c: String) -> Bool
    code = Char.toCode(c)
    match code >= 48
        true -> code <= 57
        false -> false

fn finishInt(num: String, pos: Int) -> ParseOut
    match Int.fromString(num)
        Result.Ok(n) -> ParseOut.Got(Num.NumInt(n), pos)
        Result.Err(_) -> ParseOut.Bad("bad int", pos)

fn finishNumber(s: String, start: Int, endPos: Int, asFloat: Bool) -> ParseOut
    num = String.slice(s, start, endPos)
    match asFloat
        true -> ParseOut.Bad("float unsupported", endPos)
        false -> finishInt(num, endPos)

fn scanIntTail(s: String, pos: Int, start: Int, leadingZero: Bool) -> ParseOut
    match String.charAt(s, pos)
        Option.None -> finishNumber(s, start, pos, false)
        Option.Some(c) -> match isDigit(c)
            true -> match leadingZero
                true -> ParseOut.Bad("leading zero", pos)
                false -> scanIntTail(s, pos + 1, start, false)
            false -> ParseOut.Bad("trailing", pos)

fn startDigits(s: String, start: Int, c: String) -> ParseOut
    match isDigit(c)
        true -> scanIntTail(s, start + 1, start, false)
        false -> ParseOut.Bad("expected digit", start)

fn signDigit(s: String, pos: Int, start: Int, c: String) -> ParseOut
    match isDigit(c)
        true -> scanIntTail(s, pos + 1, start, false)
        false -> ParseOut.Bad("expected digit", pos)

fn parseSign(s: String, pos: Int, start: Int) -> ParseOut
    match String.charAt(s, pos)
        Option.None -> ParseOut.Bad("expected digit", pos)
        Option.Some(c) -> match c
            "0" -> scanIntTail(s, pos + 1, start, true)
            _ -> signDigit(s, pos, start, c)

fn parseNum(s: String, start: Int) -> ParseOut
    match String.charAt(s, start)
        Option.None -> ParseOut.Bad("empty", start)
        Option.Some(c) -> match c
            "-" -> parseSign(s, start + 1, start)
            "0" -> scanIntTail(s, start + 1, start, true)
            _ -> startDigits(s, start, c)

verify parseNum law fromIntRoundtrip
    given n: Int = [-7, 0, 42, 2500]
    parseNum(render(Num.NumInt(n)), 0) => ParseOut.Got(Num.NumInt(n), String.len(render(Num.NumInt(n))))
"#;

#[test]
fn int_decimal_roundtrip_pinned_for_canonical_decimal_parser() {
    // The full canonical decimal-parser shape (json.av's
    // `parseNumber.fromIntRoundtrip` family) must pin
    // `IntDecimalRoundtrip` with every cone fn captured — the Lean
    // emission renders the fixed sign-split skeleton from these names
    // and cites the scanner's synthesized `__fuel_scan` lemma.
    let ctx = build_ctx(DECIMAL_ROUNDTRIP_SRC);
    let theorem = law_theorem(&ctx, "parseNum", "fromIntRoundtrip")
        .expect("parseNum::fromIntRoundtrip law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::IntDecimalRoundtrip {
        ref parse_fn,
        ref neg_fn,
        ref pos_fn,
        ref sign_fn,
        ref scanner_fn,
        ref predicate_fn,
        ref finish_fn,
        ref finish_int_fn,
        ref serializer_fn,
    } = theorem.strategy
    else {
        panic!(
            "canonical decimal parser must pin IntDecimalRoundtrip, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(parse_fn, "parseNum");
    assert_eq!(neg_fn, "parseSign");
    assert_eq!(pos_fn, "startDigits");
    assert_eq!(sign_fn, "signDigit");
    assert_eq!(scanner_fn, "scanIntTail");
    assert_eq!(predicate_fn, "isDigit");
    assert_eq!(finish_fn, "finishNumber");
    assert_eq!(finish_int_fn, "finishInt");
    assert_eq!(serializer_fn, "render");
}

#[test]
fn int_decimal_roundtrip_not_pinned_for_when_law() {
    // `when` premises are out of scope — the fixed proof skeleton has
    // no premise handling (mirrors the no-when gates of the sibling
    // fallbacks).
    let src = DECIMAL_ROUNDTRIP_SRC.replace(
        "    given n: Int = [-7, 0, 42, 2500]\n",
        "    given n: Int = [0, 42, 2500]\n    when n >= 0\n",
    );
    let ctx = build_ctx(&src);
    let theorem = law_theorem(&ctx, "parseNum", "fromIntRoundtrip")
        .expect("parseNum::fromIntRoundtrip law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::IntDecimalRoundtrip { .. }
        ),
        "when-law must NOT pin IntDecimalRoundtrip, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn int_decimal_roundtrip_not_pinned_when_head_dispatch_arms_deviate() {
    // Arm order is load-bearing for the emission's `split` bullets —
    // a parser with the "0" arm before the "-" arm must not pin (it
    // falls through to the prelude-simp rung's honest floor instead).
    let src = DECIMAL_ROUNDTRIP_SRC.replace(
        "            \"-\" -> parseSign(s, start + 1, start)\n            \"0\" -> scanIntTail(s, start + 1, start, true)\n",
        "            \"0\" -> scanIntTail(s, start + 1, start, true)\n            \"-\" -> parseSign(s, start + 1, start)\n",
    );
    assert_ne!(src, DECIMAL_ROUNDTRIP_SRC, "mutation must apply");
    let ctx = build_ctx(&src);
    let theorem = law_theorem(&ctx, "parseNum", "fromIntRoundtrip")
        .expect("parseNum::fromIntRoundtrip law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::IntDecimalRoundtrip { .. }
        ),
        "flipped dispatch arms must NOT pin IntDecimalRoundtrip, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn int_decimal_roundtrip_not_pinned_when_scanner_exit_deviates() {
    // The scanner's none-arm EXIT must be the canonical
    // `finish(s, start, pos, false)` continuation; an early-error exit
    // breaks the `hfin` leaf, so the detector must decline.
    let src = DECIMAL_ROUNDTRIP_SRC.replace(
        "        Option.None -> finishNumber(s, start, pos, false)\n",
        "        Option.None -> ParseOut.Bad(\"eof\", pos)\n",
    );
    assert_ne!(src, DECIMAL_ROUNDTRIP_SRC, "mutation must apply");
    let ctx = build_ctx(&src);
    let theorem = law_theorem(&ctx, "parseNum", "fromIntRoundtrip")
        .expect("parseNum::fromIntRoundtrip law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::IntDecimalRoundtrip { .. }
        ),
        "non-finish scanner exit must NOT pin IntDecimalRoundtrip, got: {:?}",
        theorem.strategy
    );
}

/// Canonical synthetic source for the `RingIdentity` detector tests —
/// a fresh-named cross-multiplied pair record (deliberately NOT the
/// `examples/data/rational.av` identifiers: the recognizer must key
/// on structure, never on names). `merge` is the non-normalizing
/// addition, `crossEq` the cross-multiplication comparator.
const RING_IDENTITY_SRC: &str = "module M\n\
     \x20   intent = \"t\"\n\
     \n\
     record Pair\n\
     \x20   lead: Int\n\
     \x20   trail: Int\n\
     \n\
     fn merge(a: Pair, b: Pair) -> Pair\n\
     \x20   Pair(lead = a.lead * b.trail + b.lead * a.trail, trail = a.trail * b.trail)\n\
     \n\
     fn crossEq(a: Pair, b: Pair) -> Bool\n\
     \x20   a.lead * b.trail == b.lead * a.trail\n\
     \n\
     verify merge law commutes\n\
     \x20   given a: Pair = [Pair(lead = 1, trail = 2), Pair(lead = -3, trail = 4)]\n\
     \x20   given b: Pair = [Pair(lead = 0, trail = 1), Pair(lead = 5, trail = -3)]\n\
     \x20   crossEq(merge(a, b), merge(b, a)) => true\n";

#[test]
fn ring_identity_pinned_for_record_cross_multiplication_law() {
    // The canonical exact-rationals shape: record givens with all-Int
    // fields, a pure {+,-,*} cone, equality by cross-multiplication
    // (`comparator(...) = true`). Every earlier rung declines
    // (LinearArithmetic rejects record givens, EnumConstantFold needs
    // constructor-pinned params, FiniteDomainCases needs closed
    // domains), so pre-strategy this fell to the prelude-simp rung's
    // caught sorry. The pin must carry the subject-first unfold list.
    let ctx = build_ctx(RING_IDENTITY_SRC);
    let theorem = law_theorem(&ctx, "merge", "commutes")
        .expect("merge::commutes law theorem missing from ProofIR");
    let aver::ir::ProofStrategy::RingIdentity { ref unfold_fns } = theorem.strategy else {
        panic!(
            "record cross-multiplication law must pin RingIdentity, got: {:?}",
            theorem.strategy
        );
    };
    assert_eq!(
        unfold_fns,
        &vec!["merge".to_string(), "crossEq".to_string()],
        "unfold list must be law subject first, then the sorted rest"
    );
}

#[test]
fn ring_identity_not_pinned_for_when_law() {
    // `when` premises are out of scope — the AC-ring simp set has no
    // premise handling; the detector mirrors the no-when gates of the
    // sibling fallbacks (EnumConstantFold / FiniteDomainCases /
    // SimpOverPreludeLemmas).
    let src = RING_IDENTITY_SRC.replace(
        "\x20   crossEq(merge(a, b), merge(b, a)) => true\n",
        "\x20   when a.trail > 0\n\
         \x20   crossEq(merge(a, b), merge(b, a)) => true\n",
    );
    assert_ne!(src, RING_IDENTITY_SRC, "mutation must apply");
    let ctx = build_ctx(&src);
    let theorem = law_theorem(&ctx, "merge", "commutes")
        .expect("merge::commutes law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::RingIdentity { .. }
        ),
        "when-law must NOT pin RingIdentity, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn ring_identity_not_pinned_without_record_given() {
    // All-Int-given polynomial identities stay OUT of the strategy's
    // scope: the family is quarantined to the multi-component record
    // carrier (cross-multiplication shape). An all-Int nonlinear
    // identity keeps today's honest path (the nonlinear wall declines
    // LinearArithmetic; the law lands on the prelude-simp rung /
    // sampled fallback) — in particular identities needing
    // coefficient collection (`t + t` vs `2 * t`), which the fixed
    // AC package cannot close.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         fn polyLeft(d: Int, x: Int, s: Int) -> Int\n\
         \x20   s * s * s * s - d * (x * (2 * s * s - d * x))\n\
         \n\
         fn polyRight(d: Int, x: Int, s: Int) -> Int\n\
         \x20   (s * s - d * x) * (s * s - d * x)\n\
         \n\
         verify polyLeft law squares\n\
         \x20   given d: Int = [1, 3]\n\
         \x20   given x: Int = [-2, 0]\n\
         \x20   given s: Int = [1, 2]\n\
         \x20   polyLeft(d, x, s) => polyRight(d, x, s)\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "polyLeft", "squares")
        .expect("polyLeft::squares law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::RingIdentity { .. }
        ),
        "all-Int-given law must NOT pin RingIdentity, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn ring_identity_not_pinned_for_order_comparison_cone() {
    // The comparator's body must be one Int `==` over arithmetic —
    // an order comparison (`>=`) is outside the ring alphabet (its
    // closure needs sign reasoning, not AC normalization), so the
    // detector must decline and leave the law on today's path.
    let src = RING_IDENTITY_SRC.replace(
        "\x20   a.lead * b.trail == b.lead * a.trail\n",
        "\x20   a.lead * b.trail >= b.lead * a.trail\n",
    );
    assert_ne!(src, RING_IDENTITY_SRC, "mutation must apply");
    let ctx = build_ctx(&src);
    let theorem = law_theorem(&ctx, "merge", "commutes")
        .expect("merge::commutes law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::RingIdentity { .. }
        ),
        "order-comparison cone must NOT pin RingIdentity, got: {:?}",
        theorem.strategy
    );
}

#[test]
fn ring_identity_not_pinned_for_single_field_record_given() {
    // Single-field records are the refinement-carrier shape (`record
    // Natural { value: Int }` + smart constructor), owned by the
    // LinearArithmetic lifted path and the Subtype/subset emit — the
    // ring detector's ≥ 2-field gate must keep them out.
    let src = "module M\n\
         \x20   intent = \"t\"\n\
         \n\
         record Box\n\
         \x20   value: Int\n\
         \n\
         fn boxAdd(a: Box, b: Box) -> Box\n\
         \x20   Box(value = a.value + b.value)\n\
         \n\
         fn boxEq(a: Box, b: Box) -> Bool\n\
         \x20   a.value == b.value\n\
         \n\
         verify boxAdd law commutes\n\
         \x20   given a: Box = [Box(value = 1), Box(value = -3)]\n\
         \x20   given b: Box = [Box(value = 0), Box(value = 5)]\n\
         \x20   boxEq(boxAdd(a, b), boxAdd(b, a)) => true\n";
    let ctx = build_ctx(src);
    let theorem = law_theorem(&ctx, "boxAdd", "commutes")
        .expect("boxAdd::commutes law theorem missing from ProofIR");
    assert!(
        !matches!(
            theorem.strategy,
            aver::ir::ProofStrategy::RingIdentity { .. }
        ),
        "single-field record given must NOT pin RingIdentity, got: {:?}",
        theorem.strategy
    );
}

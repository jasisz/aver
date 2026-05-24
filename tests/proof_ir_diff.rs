//! Step 2 safety net: ProofIR refined_types vs legacy walker.
//!
//! The Proof IR migration moves backend decisions from ad-hoc
//! shape walkers (`refinement_info_for`, `refinement_witness_for`)
//! to a single `proof_lower` producer. The risk during the
//! transition: a divergence between what the producer fills into
//! `ProofIR.refined_types` and what the legacy walkers return when
//! a backend asks for the same type. Either side missing a type,
//! or assigning a different carrier / predicate / witness, would
//! show up as a behavioural regression once Steps 3 / 4 migrate
//! the consumers.
//!
//! This test parses every flagship refinement example, runs the
//! legacy walker for each refined type it expects, and compares
//! field-by-field against the new `ProofIR` decl. Equivalence is
//! the gate; once the test is stable through Steps 3 / 4 the
//! legacy walkers' call sites can be deleted.

use aver::ast::{Spanned, TopLevel, TypeDef};
use aver::codegen::CodegenContext;
use aver::codegen::common::refinement_info_for;
use aver::codegen::recursion::{RecursionPlan, analyze_plans};
use aver::ir::proof_ir::{
    DecreaseProof, FuelMetric, Measure, PreservationProof, QuantifierType, RecursionContract,
};
use aver::source::parse_source;
use std::collections::HashSet;

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
            run_contract_lower: true,
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
        HashSet::new(),
        "diff".to_string(),
        vec![],
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }
    ctx
}

fn legacy_decision(ctx: &CodegenContext, type_name: &str) -> Option<LegacyDecl> {
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let info = refinement_info_for(type_name, &inputs)?;
    Some(LegacyDecl {
        carrier_type: info.carrier_type.to_string(),
        carrier_field: info.carrier_field.to_string(),
        predicate_param: info.param_name.to_string(),
        predicate_repr: spanned_repr(info.predicate),
    })
}

#[derive(Debug, PartialEq)]
struct LegacyDecl {
    carrier_type: String,
    carrier_field: String,
    predicate_param: String,
    predicate_repr: String,
}

/// Stable AST fingerprint for cross-check. Uses Debug output —
/// fine for this test because both legacy and new path read the
/// SAME `Spanned<Expr>` node out of the same AST.
fn spanned_repr(expr: &Spanned<aver::ast::Expr>) -> String {
    format!("{:?}", expr.node)
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
fn positive_refinement_decision_matches_legacy() {
    let src = include_str!("../examples/refinement/positive/positive.av");
    assert_equiv(src, &["Positive"]);
}

#[test]
fn int_range_refinement_decision_matches_legacy() {
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    assert_equiv(src, &["IntRange"]);
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
    let (plans, _) = analyze_plans(&inputs);

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

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("fibTR")
        .unwrap_or_else(|| panic!("fibTR has no FnContract in ProofIR"));
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
    for (lifted, legacy) in precondition.iter().zip(legacy_precondition.iter()) {
        assert_eq!(
            spanned_repr(&lifted.expr),
            spanned_repr(legacy),
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
    assert_eq!(spanned_repr(&body.base_arm_body), spanned_repr(legacy_base));
    assert_eq!(
        spanned_repr(&body.wildcard_arm_body),
        spanned_repr(legacy_wild),
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
    let (plans, _) = analyze_plans(&inputs);

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

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("exposed_count")
        .expect("exposed_count has no FnContract in ProofIR");
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
    let (plans, _) = analyze_plans(&inputs);

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

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("climb")
        .expect("climb has no FnContract");
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
    assert_eq!(spanned_repr(bound), spanned_repr(legacy_bound));
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
    let (plans, _) = analyze_plans(&inputs);

    let RecursionPlan::ListStructural {
        param_index: legacy_idx,
    } = plans
        .get("len")
        .unwrap_or_else(|| panic!("len expected as ListStructural, got: {:?}", plans))
    else {
        panic!(
            "expected legacy ListStructural, got: {:?}",
            plans.get("len")
        );
    };

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("len")
        .expect("len has no FnContract");
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

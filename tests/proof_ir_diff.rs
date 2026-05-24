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
    DecreaseProof, Measure, PreservationProof, QuantifierType, RecursionContract,
};
use aver::source::parse_source;
use std::collections::HashSet;

fn build_ctx(src: &str) -> CodegenContext {
    let mut items = parse_source(src).expect("parse");
    aver::ir::pipeline::tco(&mut items);
    let tc =
        aver::ir::pipeline::typecheck(&items, &aver::ir::TypecheckMode::Full { base_dir: None });
    assert!(tc.errors.is_empty(), "source typechecks: {:?}", tc.errors);
    aver::codegen::build_context(items, &tc, None, HashSet::new(), "diff".to_string(), vec![])
}

fn legacy_decision(ctx: &CodegenContext, type_name: &str) -> Option<LegacyDecl> {
    let info = refinement_info_for(type_name, ctx)?;
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
    assert!(refinement_info_for("Point", &ctx).is_none());
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

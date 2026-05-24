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
            run_law_lower: true,
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
    let (plans, _) = analyze_plans(&inputs);

    assert!(
        matches!(plans.get("count"), Some(RecursionPlan::SizeOfStructural)),
        "count expected as SizeOfStructural, got: {:?}",
        plans.get("count")
    );

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("count")
        .expect("count has no FnContract");
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
    let (plans, _) = analyze_plans(&inputs);

    assert!(
        matches!(plans.get("walk"), Some(RecursionPlan::StringPosAdvance)),
        "walk expected as StringPosAdvance, got: {:?}",
        plans.get("walk")
    );

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("walk")
        .expect("walk has no FnContract");
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
    let (plans, _) = analyze_plans(&inputs);

    for fn_name in ["even", "odd"] {
        assert!(
            matches!(plans.get(fn_name), Some(RecursionPlan::MutualIntCountdown)),
            "{} expected as MutualIntCountdown, got: {:?}",
            fn_name,
            plans.get(fn_name),
        );

        let contract = ctx
            .proof_ir
            .fn_contracts
            .get(fn_name)
            .unwrap_or_else(|| panic!("{} has no FnContract", fn_name));
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
    let (plans, _) = analyze_plans(&inputs);

    assert!(
        matches!(plans.get("fib"), Some(RecursionPlan::LinearRecurrence2)),
        "fib expected as LinearRecurrence2, got: {:?}",
        plans.get("fib"),
    );

    let contract = ctx
        .proof_ir
        .fn_contracts
        .get("fib")
        .expect("fib has no FnContract");
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

    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "add" && t.law_name == "commutative")
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "id" && t.law_name == "reflexive")
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "add" && t.law_name == "associative")
        .expect("add::associative law theorem missing");
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "add" && t.law_name == "identityZero")
        .expect("add::identityZero law theorem missing");
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "sub" && t.law_name == "rightIdentity")
        .expect("sub::rightIdentity law theorem missing");
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "sub" && t.law_name == "antiCommutative")
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "addOne" && t.law_name == "identityViaAdd")
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "sub" && t.law_name == "antiCommutative")
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "add" && t.law_name == "commutative")
        .expect("add::commutative law theorem missing");
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "id" && t.law_name == "identity")
        .expect("id::identity law theorem missing");
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
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_name == "touch" && t.law_name == "hasAfterSet")
        .expect("touch::hasAfterSet law theorem missing");
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

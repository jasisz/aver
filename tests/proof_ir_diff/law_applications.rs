use super::*;
use aver::ast::{Literal, Spanned};
use aver::ir::hir::{ResolvedCallee as Callee, ResolvedExpr as Expr};

const SOURCE: &str = include_str!("../fixtures/source_recursion/roundtrip.av");

#[test]
fn application_search_is_optional_repeatable_and_preserves_source_obligations() {
    use aver::codegen::{proof_lower, proof_search};
    use proof_search::ApplicationSearchBudget;

    let ctx = build_ctx(SOURCE);
    let inputs = proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let mut ir = proof_lower::lower(&inputs);
    let original = format!("{:?}", ir.law_theorems);
    assert!(
        ir.law_theorems
            .iter()
            .filter_map(|t| t.induction.as_ref())
            .flat_map(|i| &i.calls)
            .all(|c| c.applications.is_empty())
    );
    let default = ApplicationSearchBudget::default();
    let report = proof_search::populate(&inputs, &mut ir, default);
    assert!(report.applications > 0);
    assert!(!report.disabled);
    let discovered = format!("{:?}", ir.law_theorems);
    assert_eq!(proof_search::populate(&inputs, &mut ir, default), report);
    assert_eq!(format!("{:?}", ir.law_theorems), discovered);

    let limited = proof_search::populate(
        &inputs,
        &mut ir,
        ApplicationSearchBudget {
            applications: 1,
            ..default
        },
    );
    assert!(limited.limited_steps > 0);
    assert!(
        ir.law_theorems
            .iter()
            .filter_map(|t| t.induction.as_ref())
            .flat_map(|i| &i.calls)
            .all(|c| c.applications.len() <= 1)
    );

    let disabled = proof_search::populate(
        &inputs,
        &mut ir,
        ApplicationSearchBudget {
            rounds: 0,
            ..default
        },
    );
    assert!(disabled.disabled);
    assert_eq!(disabled.applications, 0);
    assert_eq!(
        format!("{:?}", ir.law_theorems),
        original,
        "Search cannot edit a claim, premise, source step, induction measure or argument"
    );
}

#[test]
fn application_search_does_not_remove_induction_for_unsupported_branch_syntax() {
    let ctx = build_ctx(
        r#"module Fallible
fn depth(value: Int) -> Result<Int, String>
    match value > 0
        true -> Result.Ok(depth(Int.div(value, 10))? + 1)
        false -> Result.Ok(0)
verify depth law reflexive
    given value: Int = [0, 1, 10]
    depth(value) => depth(value)
"#,
    );
    let plan = law_theorem(&ctx, "depth", "reflexive")
        .unwrap()
        .induction
        .as_ref()
        .expect("application search must not narrow ordinary induction");
    assert!(
        plan.calls
            .iter()
            .all(|step| step.source_step.is_none() && step.applications.is_empty())
    );
}

fn builtin(name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    Spanned::bare(Expr::Call(Callee::Builtin(name.into()), args))
}

#[test]
fn concrete_applications_retain_fixed_seeds_and_compose_distinct_functions() {
    for radix in [10, 16] {
        let source = SOURCE.replace("10", &radix.to_string());
        let ctx = build_ctx(&source);
        let theorem = law_theorem(&ctx, "digits", "roundtrip").unwrap();
        let step = &theorem.induction.as_ref().unwrap().calls[0];
        assert_eq!(
            step.arguments.len(),
            1,
            "only the value is a theorem parameter"
        );
        let value = Spanned::bare(Expr::Ident("value".into()));
        let base = Spanned::bare(Expr::Literal(Literal::Int(radix)));
        let quotient = Spanned::bare(Expr::Call(
            Callee::Intrinsic(aver::ir::hir::BuiltinIntrinsic::IntDivEuclid),
            vec![value.clone(), base.clone()],
        ));
        let remainder = Spanned::bare(Expr::Call(
            Callee::Intrinsic(aver::ir::hir::BuiltinIntrinsic::IntModEuclid),
            vec![value, base],
        ));
        let prefix = step
            .applications
            .iter()
            .find(|a| a.law_name == "prefix")
            .unwrap();
        assert_eq!(prefix.fn_id, theorem.fn_id);
        assert_eq!(
            prefix.arguments,
            vec![
                quotient.clone(),
                Spanned::bare(Expr::List(vec![remainder.clone()]))
            ]
        );
        let read = law_theorem(&ctx, "read", "suffix").unwrap();
        let suffix = step
            .applications
            .iter()
            .find(|a| a.law_name == "suffix")
            .unwrap();
        assert_eq!(suffix.fn_id, read.fn_id);
        let rest = Spanned::bare(Expr::Call(
            Callee::Fn(theorem.fn_id),
            vec![quotient, Spanned::bare(Expr::List(vec![]))],
        ));
        assert_eq!(
            suffix.arguments,
            vec![
                builtin("List.reverse", vec![rest]),
                remainder,
                Spanned::bare(Expr::Literal(Literal::Int(0)))
            ]
        );
    }
}

#[test]
fn concrete_applications_do_not_use_later_or_guarded_suppliers() {
    let (before, laws) = SOURCE.split_once("verify digits law prefix").unwrap();
    let (prefix, roundtrip) = laws.split_once("verify digits law roundtrip").unwrap();
    let reordered =
        format!("{before}verify digits law roundtrip{roundtrip}\nverify digits law prefix{prefix}");
    let conditional = SOURCE.replace(
        "    digits(value, acc) =>",
        "    when value >= 0\n    digits(value, acc) =>",
    );
    for source in [reordered, conditional] {
        let ctx = build_ctx(&source);
        let plan = law_theorem(&ctx, "digits", "roundtrip")
            .unwrap()
            .induction
            .as_ref()
            .unwrap();
        assert!(
            plan.calls
                .iter()
                .flat_map(|s| &s.applications)
                .all(|a| a.law_name != "prefix")
        );
    }
}

#[test]
fn concrete_applications_keep_import_owner_despite_colliding_local_names() {
    let left = SOURCE.replace("module Roundtrip", "module Left");
    let right = SOURCE
        .replace("module Roundtrip", "module Right")
        .replace("10", "16");
    let ctx = build_ctx_with_modules(
        "module Main\n    depends [Left, Right]\n",
        &[("Left", &left), ("Right", &right)],
    );
    assert_eq!(
        ctx.proof_ir
            .law_theorems
            .iter()
            .filter(|t| t.law_name == "roundtrip")
            .count(),
        2
    );
    for theorem in ctx
        .proof_ir
        .law_theorems
        .iter()
        .filter(|t| t.law_name == "roundtrip")
    {
        let owner = ctx.symbol_table.fn_entry(theorem.fn_id).key.scope_str();
        assert!(matches!(owner, Some("Left" | "Right")));
        let applications = &theorem.induction.as_ref().unwrap().calls[0].applications;
        assert!(applications.iter().any(|a| a.law_name == "suffix"));
        for application in applications {
            assert_eq!(
                ctx.symbol_table.fn_entry(application.fn_id).key.scope_str(),
                owner
            );
        }
    }
}

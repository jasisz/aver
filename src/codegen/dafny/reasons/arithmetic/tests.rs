use super::*;
use crate::ast::{BinOp, Literal, TopLevel, VerifyKind};
use crate::codegen::dafny::tests::ctx_from_source;

fn int(value: i64) -> Spanned<Expr> {
    Spanned::new(Expr::Literal(Literal::Int(value)), 1)
}

#[test]
fn division_discharge_matches_the_shared_literal_boundary() {
    let dynamic = Type::Result(Box::new(Type::Int), Box::new(Type::Str));
    let cases = [
        (int(2), Type::Int),
        (Spanned::new(Expr::Neg(Box::new(int(2))), 1), Type::Int),
        (
            Spanned::new(
                Expr::Literal(Literal::BigInt("100000000000000000000".to_string())),
                1,
            ),
            Type::Int,
        ),
        (int(0), dynamic.clone()),
        (
            Spanned::new(Expr::Ident("divisor".to_string()), 1),
            dynamic.clone(),
        ),
        (
            Spanned::new(
                Expr::BinOp(BinOp::Add, Box::new(int(1)), Box::new(int(1))),
                1,
            ),
            dynamic,
        ),
    ];
    for name in ["Int.div", "Int.mod"] {
        for (divisor, expected) in &cases {
            assert_eq!(
                division_result_type(name, &[int(-7), divisor.clone()]),
                Some(expected.clone())
            );
        }
        assert_eq!(division_result_type(name, &[int(1)]), None);
    }
    assert_eq!(division_result_type("Int.abs", &[int(1), int(2)]), None);
}

const SOURCE: &str = r#"fn peel(n: Int, acc: List<Int>) -> Bool
    match n > 0
        true -> Bool.and(peel(Int.div(n, 2), List.prepend(Int.mod(n, 2), acc)), n >= 0)
        false -> true

verify peel law total
    given collected: List<Int> = [[], [9]]
    given number: Int = [-1, 0, 1, 20]
    because peel(number, collected)
    using []
    peel(number, collected) holds
"#;

fn hint(source: &str) -> Option<String> {
    let ctx = ctx_from_source(source, "Quotient");
    let law = ctx
        .items
        .iter()
        .find_map(|item| match item {
            TopLevel::Verify(block) => match &block.kind {
                VerifyKind::Law(law) => Some(law.as_ref()),
                _ => None,
            },
            _ => None,
        })
        .unwrap();
    emit_quotient_calls(&law.because[0], law, "checkedStep", &ctx).map(|lines| lines.join("\n"))
}

#[test]
fn quotient_hint_reorders_givens_and_retains_growing_accumulators() {
    let text = hint(SOURCE).expect("checked quotient descent");
    assert!(text.contains("if (number > 0)"), "{text}");
    assert!(
        text.contains("checkedStep([(number % 2)] + collected, (number / 2));"),
        "{text}"
    );
    assert!(!text.contains("assume"), "{text}");
    assert!(!text.contains("requires"), "{text}");
}

#[test]
fn quotient_hint_renames_before_emission_without_capturing_inserted_names() {
    let source = SOURCE.replace("collected", "n").replace("number", "acc");
    let text = hint(&source).unwrap();
    assert!(
        text.contains("checkedStep([(acc % 2)] + n, (acc / 2));"),
        "{text}"
    );
}

#[test]
fn quotient_hint_preserves_the_actual_source_branch_guard() {
    let source = SOURCE
        .replace("match n > 0", "match n <= 0")
        .replace("true -> Bool.and", "false -> Bool.and")
        .replace("false -> true", "true -> true");
    let text = hint(&source).unwrap();
    assert!(text.contains("if !((number <= 0))"), "{text}");
}

#[test]
fn unguarded_negative_recursion_and_nonshrinking_divisor_have_no_quotient_contract() {
    assert!(hint(&SOURCE.replace("n > 0", "n != 0")).is_none());
    assert!(hint(&SOURCE.replace("Int.div(n, 2)", "Int.div(n, 1)")).is_none());
    assert!(
        hint(&SOURCE.replace("Int.div(n, 2)", "Result.withDefault(Int.div(n, 0), n)")).is_none()
    );
    let duplicate = SOURCE.replace(
        "given collected: List<Int> = [[], [9]]",
        "given collected: List<Int> = [[], [9]]\n    given extra: Int = [0]",
    );
    assert!(hint(&duplicate).is_none());
    let nested = SOURCE.replace(
        "false -> true",
        "false -> match n == 0\n            true -> true\n            false -> true",
    );
    assert!(hint(&nested).is_none());
}

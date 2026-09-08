use super::*;

const SOURCE: &str = include_str!("../fixtures/source_recursion/bounded_unfolding.av");

#[test]
fn literal_countdowns_are_obligation_local_search_hints() {
    let ctx = build_ctx(SOURCE);
    let three = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "threeDigits")
        .unwrap();
    let five = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "fiveDigits")
        .unwrap();
    assert_eq!(three.unfolding.len(), 1);
    assert_eq!(five.unfolding.len(), 2);
    for (theorem, depth) in [(three, 7), (five, 9)] {
        for plan in &theorem.unfolding {
            let plan = plan.as_ref().unwrap();
            assert_eq!(plan.depth, depth);
            let names: std::collections::BTreeSet<_> = plan
                .functions
                .iter()
                .map(|id| ctx.symbol_table.fn_entry(*id).key.name.as_str())
                .collect();
            assert_eq!(names, ["digitsInto", "readFrom"].into_iter().collect());
            assert_eq!(plan.reverse_elements, [aver::ast::Type::Int]);
        }
        assert_eq!(theorem.quantifiers.len(), 1);
        assert_eq!(theorem.premises.len(), 1);
    }
}

#[test]
fn samples_and_range_guards_do_not_supply_unfolding_widths() {
    for width in ["width", "17"] {
        let source = SOURCE
            .replace(
                "read(digits(value, 3))",
                &format!("read(digits(value, {width}))"),
            )
            .replace(
                "verify digits law threeDigits\n",
                "verify digits law threeDigits\n    given width: Int = [2, 4, 8]\n",
            );
        let ctx = build_ctx(&source);
        let law = ctx
            .proof_ir
            .law_theorems
            .iter()
            .find(|t| t.law_name == "threeDigits")
            .unwrap();
        assert!(law.unfolding[0].is_none(), "{width}: {:?}", law.unfolding);
    }
    let source = SOURCE
        .replace("[0, 1, 999]", "[123456789]")
        .replace("    when Bool.and(value >= 0, value < 1000)\n", "");
    let ctx = build_ctx(&source);
    let law = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "threeDigits")
        .unwrap();
    assert_eq!(law.unfolding[0].as_ref().unwrap().depth, 7);
    assert!(law.premises.is_empty()); // Search hints do not invent the missing bound.

    let source = SOURCE
        .replace(
            "verify digits law threeDigits\n",
            "verify digits law threeDigits\n    given suffix: List<Int> = [[], [1]]\n",
        )
        .replace(
            "read(digits(value, 3)) => value",
            "List.concat(digits(value, 3), suffix) => List.concat(digits(value, 3), suffix)",
        );
    let ctx = build_ctx(&source);
    let law = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "threeDigits")
        .unwrap();
    assert!(
        law.unfolding[0].is_none(),
        "An arbitrary suffix must retain citation search"
    );
}

#[test]
fn imported_unfolding_cones_keep_canonical_function_identity() {
    let dep = SOURCE.split("verify identity").next().unwrap().replace(
        "module BoundedUnfolding",
        "module Codec\n    exposes [digits, read]",
    );
    let ctx = build_ctx_with_modules(
        r#"module Consumer
    depends [Codec]
fn digitsInto(n: Int) -> Int
    n + 100
fn readFrom(n: Int) -> Int
    n + 200
fn roundtrip(n: Int) -> Int
    Codec.read(Codec.digits(n, 3))
verify roundtrip law fixedWidth
    given n: Int = [1]
    when Bool.and(n >= 0, n < 1000)
    using []
    roundtrip(n) => n
"#,
        &[("Codec", &dep)],
    );
    let law = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "fixedWidth")
        .unwrap();
    // A wrapper containing a composed call is deliberately outside the small
    // direct-forwarding recognizer. Put the literal call in the claim instead.
    assert!(law.unfolding[0].is_none());
    let entry = r#"module Consumer
    depends [Codec]
fn digitsInto(n: Int) -> Int
    n + 100
fn readFrom(n: Int) -> Int
    n + 200
fn identity(n: Int) -> Int
    n
verify identity law fixedWidth
    given n: Int = [1]
    when Bool.and(n >= 0, n < 1000)
    using []
    Codec.read(Codec.digits(n, 3)) => n
"#;
    let ctx = build_ctx_with_modules(entry, &[("Codec", &dep)]);
    let plan = ctx.proof_ir.law_theorems[0].unfolding[0]
        .as_ref()
        .unwrap_or_else(|| {
            panic!(
                "contracts: {:?}; recursive: {:?}",
                ctx.proof_ir.fn_contracts, ctx.recursive_fns
            )
        });
    assert_eq!(plan.functions.len(), 2);
    for id in &plan.functions {
        assert_eq!(
            ctx.symbol_table.fn_entry(*id).key.scope_str(),
            Some("Codec")
        );
    }
}

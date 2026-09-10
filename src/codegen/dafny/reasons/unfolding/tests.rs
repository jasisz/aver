use super::*;
use crate::ast::{TopLevel, VerifyKind};
use crate::codegen::dafny::tests::ctx_from_source;

const SOURCE: &str =
    include_str!("../../../../../tests/fixtures/source_recursion/bounded_unfolding.av");

fn hints(source: &str, name: &str) -> Vec<Option<String>> {
    let ctx = ctx_from_source(source, "Policy");
    let original = format!("{:?}", ctx.proof_ir);
    let (vb, law) = ctx
        .items
        .iter()
        .find_map(|item| {
            if let TopLevel::Verify(vb) = item
                && let VerifyKind::Law(law) = &vb.kind
                && law.name == name
            {
                Some((vb, law))
            } else {
                None
            }
        })
        .unwrap();
    let hints = (0..=law.because.len())
        .map(|i| attributes(vb, law, i, &ctx))
        .collect();
    assert_eq!(format!("{:?}", ctx.proof_ir), original);
    hints
}

#[test]
fn dafny_budgets_are_obligation_local_and_do_not_modify_proof_ir() {
    let three = hints(SOURCE, "threeDigits");
    let five = hints(SOURCE, "fiveDigits");
    assert_eq!(three.len(), 1);
    assert_eq!(five.len(), 2);
    for (hints, depth) in [(three, 7), (five, 9)] {
        for hint in hints {
            let hint = hint.unwrap();
            for function in ["digitsInto", "readFrom", "ListReverse<int>"] {
                assert!(
                    hint.contains(&format!("{{:fuel {function}, {depth}}}")),
                    "{hint}"
                );
            }
        }
    }
}

#[test]
fn samples_guards_and_arbitrary_sequences_do_not_become_unfolding_bounds() {
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
        assert!(hints(&source, "threeDigits")[0].is_none());
    }
    let changed = SOURCE
        .replace("[0, 1, 999]", "[123456789]")
        .replace("    when Bool.and(value >= 0, value < 1000)\n", "");
    assert_eq!(hints(&changed, "threeDigits"), hints(SOURCE, "threeDigits"));
    let sequence = SOURCE
        .replace(
            "verify digits law threeDigits\n",
            "verify digits law threeDigits\n    given suffix: List<Int> = [[], [1]]\n",
        )
        .replace(
            "read(digits(value, 3)) => value",
            "List.concat(digits(value, 3), suffix) => List.concat(digits(value, 3), suffix)",
        );
    assert!(hints(&sequence, "threeDigits")[0].is_none());
}

//! The proof kernel types every term a proof binds to a variable: a cited
//! law's given, a rule's binder, a definition's parameter, an induction
//! hypothesis, and the term a step splits on. Each script under
//! `tests/fixtures/proof_kernel_refused` proves a false or a meaningless
//! claim by binding a term of another type, and the kernel must refuse it
//! for the reason listed here. An untyped kernel accepted the scripts
//! marked "accepted before": each one proved `0 = 1`, or another claim
//! that holds of no value, from true laws.

use std::fs;
use std::path::PathBuf;

/// (script, what the kernel says, whether an untyped kernel accepted it)
const REFUSED: [(&str, &str, bool); 30] = [
    // A law about a sum, cited at an Int: `x = One.Only` for every `x` of
    // the one-value type `One`, read at 0 and at 1, gives `0 = 1`.
    (
        "law_of_a_sum_at_literals",
        "step proof/trans.0: law one.isOnly: x := Int is not One",
        true,
    ),
    (
        "law_of_a_sum_at_int_givens",
        "step proof/trans.0: law one.isOnly: x := Int is not One",
        true,
    ),
    (
        "law_at_int_through_a_definition",
        "only: x := Int is not One",
        true,
    ),
    // A law about an Int cited at a list or a text, and one about a list
    // at an Int: the claim itself applies the function to another type.
    ("int_law_at_a_list", "plusZero: List<Int> is not Int", true),
    ("int_law_at_a_text", "plusZero: String is not Int", true),
    ("list_law_at_an_int", "keep: Int is not List<Int>", true),
    (
        "law_with_when_at_a_list_given",
        "bump: List<Int> is not Int",
        true,
    ),
    (
        "law_with_when_at_a_list_computed",
        "bump: List<_> is not Int",
        false,
    ),
    (
        "law_with_when_at_a_list_linear",
        "bump: List<_> is not Int",
        false,
    ),
    (
        "law_with_when_at_a_list_no_premise",
        "bump: List<_> is not Int",
        false,
    ),
    // A split along the constructors of a sum, of a term that is an Int.
    (
        "split_of_an_int_as_a_sum",
        "step proof: pick: x := Int is not One",
        true,
    ),
    (
        "split_of_an_int_as_a_sum_through_a_call",
        "pick: x := Int is not One",
        true,
    ),
    // A split into true and false of a term that is not a Bool.
    (
        "cases_on_a_term_that_is_not_bool",
        "the term split on is not a Bool",
        true,
    ),
    (
        "cases_on_an_int_given",
        "the term split on is not a Bool",
        false,
    ),
    // An induction hypothesis over a Bool, read at an Int.
    (
        "hypothesis_of_a_bool_at_an_int",
        "hypothesis ih2: b := Int is not Bool",
        true,
    ),
    // A rule of Int arithmetic at texts, a ring step over texts, linear
    // arithmetic over Floats.
    (
        "int_rule_at_texts",
        "rule int.ne.def: a := String is not Int",
        true,
    ),
    ("ring_over_texts", "ring needs two Int terms", false),
    (
        "linear_over_floats",
        "linear arithmetic reads comparisons of Int terms",
        false,
    ),
    // `x == x` of a Float, which a NaN is a counterexample to.
    (
        "reflexive_equality_of_a_float",
        "rule bool.beq.refl: a := == on Float is not equality",
        false,
    ),
    // A type the data cannot state (`_`, a function's) is one unknown type,
    // not any type: a text or a Float passed through a definition written
    // over it does not come back as a term of every type.
    (
        "int_rule_at_texts_through_an_untyped_function",
        "f: x := String is not ?",
        false,
    ),
    (
        "reflexive_equality_of_a_float_through_an_untyped_function",
        "f: y := Float is not ?",
        false,
    ),
    // No name a script can write stands for the kernel's own notions: a
    // declared type named `_` is not a hole, and a type variable named
    // `=T` has no equality.
    (
        "int_rule_at_texts_through_a_type_named_like_the_unknown",
        "f: x := String is not _",
        false,
    ),
    (
        "reflexive_equality_of_a_float_through_a_fact_variable",
        "fact List.selfEqual: step proof: rule bool.beq.refl: a := == on ?=T is not equality",
        false,
    ),
    // A match with no arms is no term: it would have every type.
    (
        "int_rule_at_a_match_with_no_arms",
        "a match with no arms has no type",
        false,
    ),
    (
        "reflexive_equality_of_a_match_with_no_arms_as_a_float",
        "a match with no arms has no type",
        false,
    ),
    // A type variable marked as needing equality needs it wherever a
    // term reaches it: here through the given that does not carry the mark.
    (
        "equality_mark_of_a_type_variable_met_at_a_float",
        "law k: == on Float is not equality",
        false,
    ),
    // A given split by its values or as a list, which is an Int.
    (
        "enum_of_an_int_given",
        "n is not a given of finite type",
        false,
    ),
    (
        "enum_of_an_int_given_ill_typed_claim",
        "the claim compares Int is not Bool",
        false,
    ),
    (
        "listcases_on_an_int_given",
        "n is not a variable of list type",
        false,
    ),
    // A claim that has no type is refused before its proof is read.
    (
        "claim_that_has_no_type",
        "+ takes two values of one type: Int is not Bool",
        false,
    ),
];

fn fixtures() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proof_kernel_refused")
}

#[test]
fn the_kernel_refuses_a_term_bound_at_another_type() {
    for (name, why, _) in REFUSED {
        let text = fs::read_to_string(fixtures().join(format!("{name}.steps"))).unwrap();
        match aver::proof_kernel::verdict(&text) {
            Ok(law) => panic!("{name}: the kernel accepted it as a proof of {law}"),
            Err(refusal) => assert!(
                refusal.contains("step proof") && refusal.contains(why),
                "{name}: refused for another reason: {refusal}"
            ),
        }
    }
}

#[test]
fn every_refused_script_is_listed() {
    let mut on_disk: Vec<String> = fs::read_dir(fixtures())
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    on_disk.sort();
    let mut listed: Vec<String> = REFUSED
        .iter()
        .map(|(name, _, _)| format!("{name}.steps"))
        .collect();
    listed.sort();
    assert_eq!(on_disk, listed);
}

/// Twelve of the scripts prove their claim under a kernel that does not
/// type what a proof binds; the list is what this change is for.
#[test]
fn twelve_of_them_were_accepted_before() {
    assert_eq!(REFUSED.iter().filter(|(_, _, before)| *before).count(), 12);
}

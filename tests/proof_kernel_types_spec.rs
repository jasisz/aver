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
const REFUSED: [(&str, &str, bool); 53] = [
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
        "step proof: the cases are not the constructors of Int",
        true,
    ),
    (
        "split_of_an_int_as_a_sum_through_a_call",
        "the cases are not the constructors of Int",
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
    // Structural induction must cover every constructor. Omitting the one
    // counterexample used to prove that every N differs from N.B.
    (
        "induction_missing_a_constructor",
        "the induction arms are not exactly the constructors of N",
        true,
    ),
    // A script may not make equality lawful merely by setting the eq bit on
    // a record whose Float field admits NaN != NaN.
    (
        "fake_equality_on_a_record_with_float",
        "rule bool.beq.refl: a := == on R is not equality",
        true,
    ),
    // A definition may not advertise Int while its body returns String. That
    // type confusion let a script transport string concatenation through an
    // Int-returning call, use ring commutativity, and prove that concatenation
    // itself commutes.
    (
        "definition_body_has_another_type",
        "definition cast has type String, declared Int",
        true,
    ),
    // A constant declaration is another type boundary used by `const` steps;
    // lying that two String values are Ints enabled the same transport through
    // ring commutativity without any function definition.
    (
        "constant_value_has_another_type",
        "constant a has type String, declared Int",
        true,
    ),
    // A second definition with the same name supplied fake termination
    // evidence for the first, nonterminating one. Unfolding f(n) = f(n) + 1
    // and ring arithmetic then proved 0 = 1.
    (
        "duplicate_definition_spoofs_termination",
        "duplicate definition f",
        true,
    ),
    // A theorem given shadowed a module constant, but the const step ignored
    // lexical scope and asserted that the arbitrary given had the constant's
    // value.
    (
        "constant_shadowed_by_given",
        "module-level binding n is shadowed by a variable",
        true,
    ),
    // A sum and a record shared TyNamed("R"). Equality validation inspected
    // the safe sum first, while the same type could still contain a record's
    // Float field, reviving NaN != NaN through bool.beq.refl.
    (
        "sum_and_record_share_a_type_name",
        "duplicate type declaration R",
        true,
    ),
    // The termination checker treated the first `n` as the decreasing
    // parameter, while unfolding substituted the second `n`. That admitted
    // f(0, 1) = f(0, 1) + 1 and hence 0 = 1 by ring arithmetic.
    (
        "duplicate_parameter_spoofs_termination",
        "definition f has duplicate parameter n",
        true,
    ),
    // A catch-all pattern binds the whole matched value, not a proper part.
    // Treating that binder as smaller admitted f(xs) = f(xs) + 1.
    (
        "catchall_spoofs_structural_termination",
        "f recurses on something that is not a part of the value it matches",
        true,
    ),
    // Constants are equations too. A self-reference made `const a` assert
    // a = a + 1 without passing through the function termination gate.
    (
        "recursive_constant_proves_zero_is_one",
        "cyclic declaration through module-level binding a",
        true,
    ),
    // The same cycle crossed namespaces: a = f() and f() = a + 1. Neither
    // the function-only cycle check nor a constant-only one could see it.
    (
        "constant_function_cycle_proves_zero_is_one",
        "cyclic declaration through module-level binding a",
        true,
    ),
    // Record literals may omit declared fields, but finite enumeration only
    // visits complete records, so it proves a false universal equality.
    (
        "incomplete_record_breaks_finite_enumeration",
        "record R is missing field value",
        true,
    ),
    // Aver evaluates local bindings eagerly. Erasing an unused recursive
    // binding before the fallback termination check made divergence invisible.
    (
        "eager_local_recursion_is_opened",
        "f recurses from a local binding",
        true,
    ),
    // An update with the same field twice has two competing meanings: the
    // projection step read the first write while execution keeps the last.
    (
        "duplicate_record_update_field",
        "record R repeats field a",
        true,
    ),
    // A repeated declaration field gave one textual name two identities.
    (
        "duplicate_record_declaration_field",
        "record R repeats field a",
        true,
    ),
    // Outside induction, constructor patterns silently zipped their names
    // with fields. Both missing and extra names therefore passed typing.
    (
        "wrong_arity_constructor_pattern",
        "constructor pattern Option.None binds 1 names for 0 fields",
        true,
    ),
    // A constructor belongs to the sum named by its qualifier. Letting C
    // list D.Loop made D.Loop's recursive C field look structurally smaller
    // than C itself, so induction supplied `0 = 1` as its own hypothesis.
    (
        "foreign_constructor_spoofs_structural_induction",
        "constructor D.Loop does not produce C",
        true,
    ),
    // The same ownership check applies to ordinary match arms, and one sum
    // may not give the same constructor two identities.
    (
        "constructor_pattern_of_another_sum",
        "constructor D.Box does not fit C",
        true,
    ),
    (
        "duplicate_sum_constructor",
        "sum C repeats constructor C.A",
        true,
    ),
    // A typed trans conclusion could pass through an intermediate projection
    // with no type because only the written links were compared syntactically.
    (
        "untyped_intermediate_equation_in_trans",
        "Missing is not a declared record",
        true,
    ),
    // A free cited law shadowed a checked builtin fact carrying the same key,
    // so the proof could attach false content to the fact's trusted identity.
    (
        "cited_law_shadows_fact_with_same_key",
        "cited laws repeat key Int.zeroIsZero",
        true,
    ),
    // Signature lookup selected the first declaration silently, and a
    // definition silently won over a conflicting signature of the same name.
    ("duplicate_signatures", "duplicate signature f", true),
    (
        "signature_collides_with_definition",
        "definition and signature share name f",
        true,
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

/// Thirty-five scripts were accepted before their corresponding kernel gates.
#[test]
fn thirty_five_of_them_were_accepted_before() {
    assert_eq!(REFUSED.iter().filter(|(_, _, before)| *before).count(), 35);
}

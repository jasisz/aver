//! Diagnostics for two distinct types that share one bare name (#840).
//!
//! A module may declare a type whose bare name a dependency also
//! declares; the local declaration shadows the imported one, and the
//! two are distinct types. Both used to print as the same source
//! spelling, so a mismatch between them read as
//! `expects Thing, got Thing` — a message that names a type and itself
//! and sends the reader hunting for a compiler bug.
//!
//! Every "expected X, got Y" diagnostic now renders its two types
//! through one helper, which qualifies each side with its declaring
//! module when the spellings collide. Messages between two
//! differently-named types are untouched, which the control tests at
//! the bottom pin.

use aver::source::{LoadedModule, parse_source};
use aver::types::checker::run_type_check_with_loaded;
use std::path::PathBuf;

const ALPHA_SRC: &str = r#"module Alpha
    intent = "Dependency that exposes its own Thing."
    exposes [mkThing, mkThings, Thing]
    effects []

record Thing
    tag: String

fn mkThing(t: String) -> Thing
    ? "Build the dependency's Thing."
    Thing(tag = t)

fn mkThings() -> List<Thing>
    ? "Build a list of the dependency's Thing."
    [Thing(tag = "x")]
"#;

fn check_errors(entry_source: &str) -> Vec<String> {
    let entry_items = parse_source(entry_source).expect("entry parse");
    let loaded = vec![LoadedModule {
        dep_name: "Alpha".to_string(),
        items: parse_source(ALPHA_SRC).expect("Alpha parse"),
        path: PathBuf::from("alpha.av"),
    }];
    run_type_check_with_loaded(&entry_items, &loaded)
        .errors
        .into_iter()
        .map(|e| e.message)
        .collect()
}

fn assert_contains(errors: &[String], needle: &str) {
    assert!(
        errors.iter().any(|e| e.contains(needle)),
        "expected an error containing {needle:?}, got: {errors:#?}"
    );
}

#[test]
fn record_field_mismatch_names_both_declaring_modules() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with its own Thing."
    depends [Alpha]
    effects []

record Thing
    value: Int

record Holder
    item: Thing

fn probe() -> Holder
    ? "Build a Holder from the dependency's Thing."
    Holder(item = Alpha.mkThing("x"))
"#,
    );
    assert_contains(
        &errors,
        "Record 'Holder' field 'item' expects Main.Thing, got Alpha.Thing",
    );
}

#[test]
fn argument_mismatch_names_both_declaring_modules() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with its own Thing."
    depends [Alpha]
    effects []

record Thing
    value: Int

fn take(t: Thing) -> Int
    ? "Read the entry Thing's value."
    t.value

fn probe() -> Int
    ? "Pass the dependency's Thing where the entry's is expected."
    take(Alpha.mkThing("x"))
"#,
    );
    assert_contains(
        &errors,
        "Argument 1 of 'take': expected Main.Thing, got Alpha.Thing",
    );
}

#[test]
fn binding_annotation_mismatch_names_both_declaring_modules() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with its own Thing."
    depends [Alpha]
    effects []

record Thing
    value: Int

fn probe() -> Int
    ? "Annotate a dependency Thing with the entry's Thing."
    t: Thing = Alpha.mkThing("x")
    t.value
"#,
    );
    assert_contains(
        &errors,
        "Binding 't': expression has type Alpha.Thing, annotation says Main.Thing",
    );
}

#[test]
fn collision_inside_a_generic_qualifies_the_inner_type() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with its own Thing."
    depends [Alpha]
    effects []

record Thing
    value: Int

record Holder
    items: List<Thing>

fn probe() -> Holder
    ? "Fill a Holder from the dependency's Things."
    Holder(items = Alpha.mkThings())
"#,
    );
    assert_contains(
        &errors,
        "Record 'Holder' field 'items' expects List<Main.Thing>, got List<Alpha.Thing>",
    );
}

#[test]
fn list_element_mismatch_names_both_declaring_modules() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with its own Thing."
    depends [Alpha]
    effects []

record Thing
    value: Int

fn take(ts: List<Thing>) -> Int
    ? "Count the entry Things."
    List.len(ts)

fn probe() -> Int
    ? "Pass a list literal holding the dependency's Thing."
    take([Alpha.mkThing("x")])
"#,
    );
    assert_contains(
        &errors,
        "List element 1: expected Main.Thing, got Alpha.Thing",
    );
}

#[test]
fn matching_types_from_one_module_still_check() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry that uses the dependency's Thing only."
    depends [Alpha]
    effects []

record Holder
    item: Alpha.Thing

fn probe() -> Holder
    ? "Build a Holder from the dependency's Thing."
    Holder(item = Alpha.mkThing("x"))
"#,
    );
    assert!(errors.is_empty(), "expected no errors, got: {errors:#?}");
}

#[test]
fn ordinary_mismatch_keeps_its_wording() {
    let errors = check_errors(
        r#"module Main
    intent = "Entry with an ordinary type mismatch."
    depends [Alpha]
    effects []

record Holder
    count: Int

fn take(n: Int) -> Int
    ? "Return the argument."
    n

fn probe() -> Holder
    ? "Mismatch two differently named types."
    Holder(count = "x")

fn probeArg() -> Int
    ? "Mismatch two differently named types in argument position."
    take("x")

fn probeBinding() -> Int
    ? "Mismatch two differently named types in a binding annotation."
    n: Int = "x"
    n
"#,
    );
    assert_contains(
        &errors,
        "Record 'Holder' field 'count' expects Int, got String",
    );
    assert_contains(&errors, "Argument 1 of 'take': expected Int, got String");
    assert_contains(
        &errors,
        "Binding 'n': expression has type String, annotation says Int",
    );
}

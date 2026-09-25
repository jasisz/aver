//! `warning[perf-shared-update]`: a repeated in-place update of a Map or
//! Vector that a record still holds.
//!
//! Each shape below is one decision the check makes. The positive cases are
//! the two ways the btc-listener port copied a whole window per Block: a
//! field of a record handed to a function that updates it while the record
//! is still needed, and the same record already handed whole to the enclosing
//! call. The negative cases are what keeps the warning trustworthy.

use aver::diagnostics::analyze::{AnalyzeOptions, analyze_source};

const SLUG: &str = "perf-shared-update";

fn warnings_in(source: &str, root: Option<&std::path::Path>) -> Vec<String> {
    let mut options = AnalyzeOptions::new("main.av");
    if let Some(root) = root {
        options.module_base_dir = Some(root.to_string_lossy().to_string());
    }
    let report = analyze_source(source, &options);
    let errors: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|d| d.is_error() && d.slug != "missing-verify")
        .map(|d| d.summary.clone())
        .collect();
    assert!(errors.is_empty(), "the program should check: {errors:?}");
    report
        .diagnostics
        .iter()
        .filter(|d| d.slug == SLUG)
        .map(|d| d.summary.clone())
        .collect()
}

fn warnings(source: &str) -> Vec<String> {
    warnings_in(source, None)
}

const HEADER: &str = r#"module Main
    intent = "shared updates"
    effects []

record Window
    created: Map<Int, Int>
    held: Int

record Setting
    window: Window
    height: Int

fn absorbed(created: Map<Int, Int>, key: Int) -> Map<Int, Int>
    ? "One more key in the window."
    Map.set(created, key, 1)

fn absorbedInto(setting: Setting, created: Map<Int, Int>) -> Setting
    ? "The window moved on."
    Setting.update(setting, window = Window(created = created, held = setting.window.held + 1), height = setting.height + 1)
"#;

fn program(body: &str) -> String {
    format!("{HEADER}\n{body}")
}

/// The btc shape: the record is the first argument of the enclosing call, so
/// it still holds the Map while the inner call updates it.
#[test]
fn a_field_updated_while_the_record_is_an_earlier_argument_warns() {
    let found = warnings(&program(
        r#"
fn step(setting: Setting, left: Int) -> Setting
    ? "One Block per step."
    match left <= 0
        true -> setting
        false -> step(absorbedInto(setting, absorbed(setting.window.created, left)), left - 1)
"#,
    ));
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(
        found[0].contains("`absorbed` updates `setting.window.created`, a Map that is still held by `setting`; each call copies the whole Map"),
        "{}",
        found[0]
    );
}

/// The record is read again after the call that updates its field.
#[test]
fn a_field_updated_while_the_record_is_read_later_warns() {
    let found = warnings(&program(
        r#"
fn step(setting: Setting, left: Int) -> Setting
    ? "One Block per step."
    match left <= 0
        true -> setting
        false -> step(absorbedInto(Setting.update(setting, height = 0), absorbed(setting.window.created, left)), left - 1)

fn stepAgain(setting: Setting, left: Int) -> Setting
    ? "The same, with the Map set here."
    grown = Map.set(setting.window.created, left, 2)
    match left <= 0
        true -> setting
        false -> stepAgain(absorbedInto(setting, grown), left - 1)
"#,
    ));
    assert_eq!(found.len(), 2, "{found:?}");
    assert!(found.iter().any(|w| w.starts_with(
        "`Map.set` on `setting.window.created` updates a Map that is still held by `setting`"
    )));
}

/// The parts are moved out of the record first (the btc fix): nothing warns.
#[test]
fn parts_moved_out_of_the_record_do_not_warn() {
    let found = warnings(&program(
        r#"
fn emptied(setting: Setting) -> Setting
    ? "The Setting without its window."
    Setting.update(setting, window = Window(created = {}, held = 0))

fn step(setting: Setting, left: Int) -> Setting
    ? "One Block per step."
    match left <= 0
        true -> setting
        false -> match setting.window.created
            created -> step(absorbedInto(emptied(setting), absorbed(created, left)), left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// Read at the record's last use, the field is not shared by anything the
/// function can see.
#[test]
fn a_field_read_at_the_records_last_use_does_not_warn() {
    let found = warnings(&program(
        r#"
fn step(setting: Setting, left: Int) -> Map<Int, Int>
    ? "The window, grown."
    match left <= 0
        true -> setting.window.created
        false -> absorbed(setting.window.created, left)

fn loop(setting: Setting, left: Int) -> Int
    ? "Steps."
    match left <= 0
        true -> 0
        false -> Map.len(step(setting, left)) + loop(Setting(window = Window(created = {}, held = 0), height = 0), left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// A later argument of the same call that reads the record is over before
/// the callee runs.
#[test]
fn a_record_read_in_a_later_argument_does_not_warn() {
    let found = warnings(&program(
        r#"
fn grown(created: Map<Int, Int>, key: Int, height: Int) -> Map<Int, Int>
    ? "One more key."
    Map.set(created, key, height)

fn step(setting: Setting, left: Int) -> Setting
    ? "One Block per step."
    match left <= 0
        true -> setting
        false -> step(Setting(window = Window(created = grown(setting.window.created, left, setting.height), held = 0), height = 0), left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// A function that reads the Map but hands nothing back is not an update the
/// caller could have avoided.
#[test]
fn a_callee_that_does_not_hand_the_collection_back_does_not_warn() {
    let found = warnings(&program(
        r#"
fn sizeWith(created: Map<Int, Int>, key: Int) -> Int
    ? "The size the window would have with one more key."
    Map.len(Map.set(created, key, 1))

fn step(setting: Setting, left: Int) -> Int
    ? "Sizes."
    match left <= 0
        true -> setting.height
        false -> sizeWith(setting.window.created, left) + step(setting, left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// Not repeated: a function no loop reaches copies once.
#[test]
fn an_update_outside_any_loop_does_not_warn() {
    let found = warnings(&program(
        r#"
fn once(setting: Setting) -> Setting
    ? "One Block."
    absorbedInto(setting, absorbed(setting.window.created, 1))
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// A bare local that is read again keeps both versions on purpose.
#[test]
fn a_bare_local_kept_after_the_update_does_not_warn() {
    let found = warnings(&program(
        r#"
fn scoped(created: Map<Int, Int>, left: Int) -> Int
    ? "A copy per scope, and the outer one kept."
    inner = Map.set(created, left, 1)
    match left <= 0
        true -> Map.len(inner)
        false -> Map.len(inner) + scoped(created, left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// The update reads the field it replaces and otherwise only other fields:
/// the VM takes the field out of the record first.
#[test]
fn a_record_update_that_consumes_the_record_does_not_warn() {
    let found = warnings(&program(
        r#"
fn step(window: Window, left: Int) -> Window
    ? "One key per step."
    match left <= 0
        true -> window
        false -> step(Window.update(window, created = Map.set(window.created, left, 1), held = window.held + 1), left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// The same update with the whole record read inside it is a copy.
#[test]
fn a_record_update_that_also_reads_the_whole_record_warns() {
    let found = warnings(&program(
        r#"
fn heldOf(window: Window) -> Int
    ? "How many keys the window holds."
    window.held

fn step(window: Window, left: Int) -> Window
    ? "One key per step."
    match left <= 0
        true -> window
        false -> step(Window.update(window, created = Map.set(window.created, left, heldOf(window))), left - 1)
"#,
    ));
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].contains("still held by `window`"), "{}", found[0]);
}

/// A field one record down, set inside an update of the inner record inside
/// an update of the outer one: the VM takes it out first.
#[test]
fn an_update_of_an_update_that_consumes_the_record_does_not_warn() {
    let found = warnings(&program(
        r#"
fn step(setting: Setting, left: Int) -> Setting
    ? "One key per step."
    match left <= 0
        true -> setting
        false -> step(Setting.update(setting, window = Window.update(setting.window, created = Map.set(setting.window.created, left, 1)), height = setting.height + 1), left - 1)

fn stepNew(setting: Setting, left: Int) -> Setting
    ? "The same, building a new window."
    match left <= 0
        true -> setting
        false -> stepNew(Setting(window = Window(created = Map.set(setting.window.created, left, 1), held = setting.window.held), height = setting.height + 1), left - 1)
"#,
    ));
    assert!(found.is_empty(), "{found:?}");
}

/// The same with the inner Map read a second time is a copy.
#[test]
fn an_update_of_an_update_that_reads_the_map_twice_warns() {
    let found = warnings(&program(
        r#"
fn step(setting: Setting, left: Int) -> Setting
    ? "One key per step."
    match left <= 0
        true -> setting
        false -> step(Setting.update(setting, window = Window.update(setting.window, created = Map.set(setting.window.created, left, Map.len(setting.window.created))), height = setting.height + 1), left - 1)
"#,
    ));
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].contains("still held by `setting`"), "{}", found[0]);
}

/// The callee lives in a dependency: the check reads its source to see that
/// it updates the Map and hands it back.
#[test]
fn a_callee_in_a_dependency_is_followed() {
    let dir = tempfile::tempdir().expect("module root");
    std::fs::write(
        dir.path().join("utxo.av"),
        r#"module Utxo
    intent = "the window's updates"
    exposes [absorbed]
    effects []

fn absorbed(created: Map<Int, Int>, key: Int) -> Map<Int, Int>
    ? "One more key, through a helper."
    madeInto(created, key)

fn madeInto(created: Map<Int, Int>, key: Int) -> Map<Int, Int>
    ? "The key set."
    Map.set(created, key, 1)
"#,
    )
    .expect("write the dependency");
    let source = r#"module Main
    intent = "shared updates across modules"
    depends [Utxo]
    effects []

record Setting
    created: Map<Int, Int>
    height: Int

fn absorbedInto(setting: Setting, created: Map<Int, Int>) -> Setting
    ? "The window moved on."
    Setting.update(setting, created = created, height = setting.height + 1)

fn step(setting: Setting, left: Int) -> Setting
    ? "One Block per step."
    match left <= 0
        true -> setting
        false -> step(absorbedInto(setting, Utxo.absorbed(setting.created, left)), left - 1)
"#;
    let found = warnings_in(source, Some(dir.path()));
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(
        found[0].starts_with(
            "`Utxo.absorbed` updates `setting.created`, a Map that is still held by `setting`"
        ),
        "{}",
        found[0]
    );
}

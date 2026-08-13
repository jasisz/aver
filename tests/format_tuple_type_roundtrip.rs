//! Regression — a type annotation naming a tuple must survive `aver
//! format` and still pass `aver check`.
//!
//! `Tuple<A, B>` is the only spelling for a tuple type; the paren form
//! `(A, B)` was removed from type position and the parser rejects it.
//! The formatter re-printed every annotation it could parse, and its
//! private type printer still emitted the removed paren spelling for
//! `Type::Tuple`. So `aver format` rewrote a header the checker had
//! just accepted into one the parser refuses, then failed on its own
//! output — reporting the parser's message, which names `Tuple<A, B>`
//! as the repair the source already used.

use std::fs;
use std::process::Command;

const BIN: &str = env!("CARGO_BIN_EXE_aver");

fn workdir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("aver_format_tuple_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn run(args: &[&str]) -> (Option<i32>, String) {
    let out = Command::new(BIN).args(args).output().expect("run aver");
    let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
    text.push_str(&String::from_utf8_lossy(&out.stderr));
    (out.status.code(), text)
}

/// Write `source`, assert `aver check` accepts it, run `aver format`,
/// then assert the formatted file still passes `aver check` and that
/// `aver format --check` reports it clean (the format is a fixed
/// point). Returns the formatted text.
fn round_trip(name: &str, source: &str) -> String {
    let dir = workdir(name);
    let file = dir.join(format!("{name}.av"));
    fs::write(&file, source).expect("write source");
    let path = file.to_str().expect("utf-8 path");

    let (code, out) = run(&["check", path]);
    assert_eq!(code, Some(0), "`aver check` must accept the source:\n{out}");

    let (code, out) = run(&["format", path]);
    assert_eq!(
        code,
        Some(0),
        "`aver format` must accept what `aver check` accepted:\n{out}"
    );

    let formatted = fs::read_to_string(&file).expect("read formatted");

    let (code, out) = run(&["check", path]);
    assert_eq!(
        code,
        Some(0),
        "formatted output must still pass `aver check`:\n{out}\n--- formatted ---\n{formatted}"
    );

    let (code, out) = run(&["format", "--check", path]);
    assert_eq!(
        code,
        Some(0),
        "formatting must be a fixed point:\n{out}\n--- formatted ---\n{formatted}"
    );

    formatted
}

/// The issue's reproduction: a tuple named as a parameter type.
#[test]
fn tuple_parameter_type_survives_format() {
    let formatted = round_trip(
        "param",
        r#"module TupParam
    intent = "A bare tuple as a parameter type."
    exposes [first]
    effects []

fn first(pair: Tuple<String, String>) -> String
    ? "The left half."
    match pair
        (a, b) -> a

verify first
    first(("a", "b")) => "a"
"#,
    );
    assert!(
        formatted.contains("pair: Tuple<String, String>"),
        "formatter must keep the `Tuple<A, B>` spelling:\n{formatted}"
    );
}

/// A tuple in return position goes through the same printer.
#[test]
fn tuple_return_type_survives_format() {
    let formatted = round_trip(
        "ret",
        r#"module TupRet
    intent = "A bare tuple as a return type."
    exposes [pairUp]
    effects []

fn pairUp(a: String, b: String) -> Tuple<String, String>
    ? "Both halves."
    (a, b)

verify pairUp
    pairUp("a", "b") => ("a", "b")
"#,
    );
    assert!(
        formatted.contains("-> Tuple<String, String>"),
        "formatter must keep the `Tuple<A, B>` spelling in return position:\n{formatted}"
    );
}

/// The shape the issue was met through — `Map.entries` hands back
/// `List<Tuple<K, V>>`, so anything consuming it names a nested tuple.
#[test]
fn nested_tuple_type_survives_format() {
    let formatted = round_trip(
        "nested",
        r#"module TupNested
    intent = "A tuple nested inside a list parameter type."
    exposes [countPairs]
    effects []

fn countPairs(pairs: List<Tuple<String, Int>>) -> Int
    ? "How many pairs there are."
    List.len(pairs)

verify countPairs
    countPairs([("a", 1)]) => 1
"#,
    );
    assert!(
        formatted.contains("pairs: List<Tuple<String, Int>>"),
        "formatter must keep the nested `Tuple<A, B>` spelling:\n{formatted}"
    );
}

/// Control — the formatter still normalizes annotations it re-prints
/// (whitespace inside the type arguments is canonicalized), so the fix
/// did not turn the type printer off.
#[test]
fn type_annotations_are_still_normalized() {
    let formatted = round_trip(
        "normalize",
        r#"module TupNormalize
    intent = "Sloppy spacing inside a type argument list."
    exposes [first]
    effects []

fn first(pair: Tuple<String,String>) -> String
    ? "The left half."
    match pair
        (a, b) -> a

verify first
    first(("a", "b")) => "a"
"#,
    );
    assert!(
        formatted.contains("pair: Tuple<String, String>"),
        "formatter must still canonicalize spacing inside type arguments:\n{formatted}"
    );
}

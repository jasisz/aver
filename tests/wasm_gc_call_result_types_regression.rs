#![cfg(feature = "wasm")]

//! Regression: a composite value whose type only exists as the result of a
//! call in a function body.
//!
//! wasm-gc registered `List` / `Option` / `Result` / `Map` / `Vector` / tuple
//! instantiations from signatures, record fields, binding annotations and a
//! table of builtin calls. `List.len(List.zip([1], ["a"]))` or
//! `Option.Some(1) == Option.None` written straight in `main` names a type
//! none of those spell, so the module failed validation for want of the slot
//! or its helpers, while the VM ran the program. The backend now registers
//! every type the checker stamped on a body expression.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_command, cleanup, format_output, repo_root, temp_module};

const PROGRAM: &str = r#"module CallResults
    intent =
        "Composite types that only exist as the result of a call inside main."
    effects [Console]

fn pairedCount(n: Int) -> Int
    ? "Counts pairs of a zip whose list type no signature names."
    List.len(List.zip([n, n + 1], [Option.Some(n), Option.None]))

fn main() -> Unit
    ! [Console.print]
    zipLen = List.len(List.zip([1, 2, 3], ["a", "b"]))
    Console.print("zip len: {zipLen}")
    nestedLen = List.len(List.zip(List.zip([1], [true]), ["x", "y"]))
    Console.print("nested zip len: {nestedLen}")
    Console.print("option eq: {Option.Some(1) == Option.None}")
    Console.print("option eq same: {Option.Some(2) == Option.Some(2)}")
    resultEq = Result.fromOption(Option.Some(1.5), "no") == Result.fromOption(Option.Some(2.5), "no")
    Console.print("result eq: {resultEq}")
    fromOpt = Result.withDefault(Result.fromOption(Option.Some(true), "none"), false)
    Console.print("result from option: {fromOpt}")
    ages = Map.fromList([("alice", 30), ("bob", 25)])
    Console.print("map get: {Option.withDefault(Map.get(ages, "bob"), 0)}")
    Console.print("map keys: {List.len(Map.keys(ages))}")
    missing = Map.get(ages, "carol") == Option.None
    Console.print("map get eq: {missing}")
    scores = Map.fromList(List.zip([1, 2], [1.5, 2.5]))
    Console.print("zip map len: {Map.len(scores)}")
    v = Vector.fromList(["p", "q", "r"])
    Console.print("vector get: {Option.withDefault(Vector.get(v, 2), "-")}")
    Console.print("vector get eq: {Vector.get(v, 9) == Option.None}")
    Console.print("vector to list: {List.len(List.fromVector(v))}")
    entries = Map.entries(Map.fromList([(true, [1, 2])]))
    Console.print("entries: {List.len(entries)}")
    Console.print("helper zip: {pairedCount(4)}")
"#;

const EXPECTED: &str = "zip len: 2\nnested zip len: 1\noption eq: false\noption eq same: true\nresult eq: false\nresult from option: true\nmap get: 25\nmap keys: 2\nmap get eq: true\nzip map len: 2\nvector get: r\nvector get eq: true\nvector to list: 3\nentries: 1\nhelper zip: 2\n";

fn run(prefix: &str, args: &[&str]) -> String {
    let path = temp_module(prefix, PROGRAM);
    let out = aver_command()
        .current_dir(repo_root())
        .args(args)
        .arg(&path)
        .output()
        .expect("aver executes");
    cleanup(&path);
    assert!(out.status.success(), "{}", format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn call_result_types_run_on_the_vm() {
    assert_eq!(run("call-results-vm", &["run"]), EXPECTED);
}

#[test]
fn call_result_types_run_on_wasm_gc() {
    assert_eq!(run("call-results-wg", &["run", "--wasm-gc"]), EXPECTED);
}

//! Versions of one wasm-gc map.
//!
//! `Map.set` and `Map.remove` write into the map's arrays in place and
//! leave the map they were given valid through a diff (see
//! `src/codegen/wasm_gc/maps/versions.rs`). These tests keep every older
//! version in use after newer ones are made from it: across updates,
//! inserts, removes that shift a probe run, growth, a long chain of
//! overwrites, two branches from one base, and equality between versions
//! that share arrays. Each program runs on the VM and on wasm-gc, and both
//! must print the hand-checked answer.
//!
//! Before versions, `Map.remove` wrote into the map it was given, so a
//! caller that kept that map saw the key disappear, and `Map.set` copied
//! the whole table on every call whose receiver it could not prove unique
//! (an answer module's state field, for one).

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{cleanup, format_output, temp_module};

use std::path::PathBuf;
use std::process::Command;

fn run_cli(prefix: &str, source: &str, extra_args: &[&str]) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .args(extra_args)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{prefix} run {extra_args:?} failed:\n{}",
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

fn assert_vm_and_wasm_gc(name: &str, source: &str, expected: &str) {
    let vm = run_cli(name, source, &[]);
    assert_eq!(
        vm, expected,
        "{name}: the VM diverged from the checked answer"
    );
    let wasm = run_cli(name, source, &["--wasm-gc"]);
    assert_eq!(
        wasm, expected,
        "{name}: wasm-gc diverged from the checked answer — a version of a map \
         saw a write made to a later one"
    );
}

#[test]
fn remove_leaves_the_map_it_was_given_alone() {
    assert_vm_and_wasm_gc(
        "map-versions-remove",
        r#"
module Main
    intent = "Remove and set from one map, then read all three."
    effects [Console.print]

fn both(m: Map<Int, Int>) -> Tuple<Map<Int, Int>, Map<Int, Int>>
    ? "Two maps from one."
    (Map.remove(m, 1), Map.set(m, 2, 20))

fn main() -> Unit
    ! [Console.print]
    base = Map.set(Map.set({}, 1, 10), 3, 30)
    pair = both(base)
    match pair
        (a, b) -> Console.print("{Map.len(base)} {Map.has(base, 1)} {Map.len(a)} {Map.has(a, 1)} {Map.len(b)} {Map.has(b, 1)} {Map.has(b, 2)} {Map.has(base, 2)}")
"#,
        "2 true 1 false 3 true true false",
    );
}

#[test]
fn every_version_reads_as_itself() {
    assert_vm_and_wasm_gc(
        "map-versions-all",
        r#"
module Main
    intent = "Every version of a map stays what it was when the next one is made from it."
    effects [Console.print]

fn fill(m: Map<Int, Int>, from: Int, to: Int) -> Map<Int, Int>
    ? "Maps each key in from..to-1 to ten times itself."
    match from >= to
        true -> m
        false -> fill(Map.set(m, from, from * 10), from + 1, to)

fn total(m: Map<Int, Int>, keys: List<Int>, acc: Int) -> Int
    ? "Sums key * 1000 + value over the listed keys, and counts an absent one as -1."
    match keys
        [] -> acc
        [k, ..rest] -> match Map.get(m, k)
            Option.Some(v) -> total(m, rest, acc + k * 1000 + v)
            Option.None -> total(m, rest, acc - 1)

fn summary(m: Map<Int, Int>) -> String
    ? "Size, a checksum of the entries, and the first keys in order."
    "{Map.len(m)}:{total(m, Map.keys(m), 0)}:{firstKeys(List.take(Map.keys(m), 4), "")}"

fn firstKeys(keys: List<Int>, acc: String) -> String
    ? "The keys, comma-separated."
    match keys
        [] -> acc
        [k, ..rest] -> firstKeys(rest, "{acc},{k}")

fn chain(m: Map<Int, Int>, left: Int) -> Map<Int, Int>
    ? "Overwrites keys left down to 1 with 7."
    match left <= 0
        true -> m
        false -> chain(Map.set(m, left, 7), left - 1)

fn removeAll(m: Map<Int, Int>, keys: List<Int>) -> Map<Int, Int>
    ? "Removes every listed key."
    match keys
        [] -> m
        [k, ..rest] -> removeAll(Map.remove(m, k), rest)

fn main() -> Unit
    ! [Console.print]
    base = fill({}, 0, 40)
    grown = fill(base, 40, 100)
    updated = Map.set(base, 3, 999)
    other = Map.set(base, 3, 111)
    removed = removeAll(base, [0, 16, 32, 5, 21, 37, 99])
    long = chain(base, 30)
    Console.print(summary(base))
    Console.print(summary(grown))
    Console.print(summary(updated))
    Console.print(summary(other))
    Console.print(summary(removed))
    Console.print(summary(long))
    Console.print(summary(base))
    Console.print("{base == fill({}, 0, 40)} {updated == other} {Map.set(base, 3, 30) == base} {removed == base} {Map.has(removed, 16)} {Map.has(base, 16)}")
"#,
        "40:787800:,0,1,2,3\n\
         100:4999500:,0,1,2,3\n\
         40:788769:,0,1,2,3\n\
         40:787881:,0,1,2,3\n\
         34:675690:,1,2,3,4\n\
         40:783360:,0,1,2,3\n\
         40:787800:,0,1,2,3\n\
         true false true false false true",
    );
}

/// The printed WAT of one `aver compile --target wasm-gc` of `source`.
fn wat_of(source: &str) -> String {
    let mut items = aver::source::parse_source(source).expect("parse");
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(aver::ir::TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_list_build: false,
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, result.analysis.as_ref())
        .expect("wasm-gc compile");
    wasmprinter::print_bytes(&bytes).expect("print wat")
}

/// An answer module's state reaches `Map.set` as a field of a record the
/// function was handed, which no ownership fact covers. The insert must
/// still not copy the table: its only array allocations are the grow's, and
/// it never copies an array.
#[test]
fn set_on_a_record_field_does_not_copy_the_table() {
    let wat = wat_of(
        r#"
module Main
    intent = "A state record whose map every request updates."
    effects [Console.print]

record State
    counts: Map<Int, Int>

fn bump(state: State, key: Int) -> State
    ? "One more for this key."
    next = Option.withDefault(Map.get(state.counts, key), 0) + 1
    State(counts = Map.set(state.counts, key, next))

verify bump
    bump(State(counts = {}), 7) => State(counts = {7 => 1})

fn main() -> Unit
    ! [Console.print]
    Console.print("{Map.len(bump(State(counts = {}), 1).counts)}")
"#,
    );
    let start = wat
        .find("(func $\"Map.set Map<Int,Int>")
        .unwrap_or_else(|| panic!("no named Map.set helper in:\n{wat}"));
    let body = &wat[start..];
    let body = &body[..body[1..]
        .find("\n  (func")
        .map_or(body.len(), |end| end + 1)];
    assert!(
        !body.contains("array.copy"),
        "Map.set copies an array:\n{body}"
    );
    assert_eq!(
        body.matches("array.new_default").count(),
        3,
        "Map.set allocates arrays outside its grow:\n{body}"
    );
}

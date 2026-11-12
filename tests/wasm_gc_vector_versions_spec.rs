//! Versions of one wasm-gc Vector.
//!
//! `Vector.set` writes the cell in place and leaves the vector it was given
//! valid through a diff (see `src/codegen/wasm_gc/vectors.rs`). These tests
//! keep older versions in use after newer ones are made from them: a chain of
//! writes read back in any order, two branches from one base, equality
//! between versions that share an array, a builder loop whose base is kept,
//! and a Vector in a record field that the caller keeps. Each program runs on
//! the VM and on wasm-gc, and both must print the hand-checked answer.
//!
//! Before versions, `Vector.set` copied the whole array whenever it could not
//! prove its receiver unique, which it cannot for a Vector held in a record
//! field.

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
        "{name}: wasm-gc diverged from the checked answer — a version of a vector \
         saw a write made to another one"
    );
}

const AT: &str = r#"
fn at(v: Vector<Int>, i: Int) -> Int
    ? "The cell, or -1 outside the vector."
    Option.withDefault(Vector.get(v, i), 0 - 1)

fn put(v: Vector<Int>, i: Int, x: Int) -> Vector<Int>
    ? "The vector with one cell written, or the vector itself outside it."
    match Vector.set(v, i, x)
        Option.Some(w) -> w
        Option.None -> v

fn show(v: Vector<Int>) -> String
    ? "The cells, comma-separated."
    showFrom(List.fromVector(v), "")

fn showFrom(cells: List<Int>, acc: String) -> String
    ? "The cells after acc."
    match cells
        [] -> acc
        [c, ..rest] -> showFrom(rest, "{acc}{c},")
"#;

fn program(body: &str) -> String {
    format!(
        "module Main\n    intent = \"Versions of one vector.\"\n    effects [Console.print]\n{AT}\n{body}"
    )
}

#[test]
fn every_version_of_a_chain_reads_as_itself() {
    assert_vm_and_wasm_gc(
        "vector-versions-chain",
        &program(
            r#"
fn main() -> Unit
    ! [Console.print]
    v0 = Vector.new(4, 0)
    v1 = put(v0, 0, 1)
    v2 = put(v1, 1, 2)
    v3 = put(v2, 0, 3)
    v4 = put(v3, 3, 4)
    Console.print("{show(v0)} {show(v2)} {show(v4)} {show(v1)} {show(v3)} {show(v0)} {show(v4)}")
"#,
        ),
        "0,0,0,0, 1,2,0,0, 3,2,0,4, 1,0,0,0, 3,2,0,0, 0,0,0,0, 3,2,0,4,",
    );
}

#[test]
fn two_branches_from_one_base_stay_apart() {
    assert_vm_and_wasm_gc(
        "vector-versions-branches",
        &program(
            r#"
fn main() -> Unit
    ! [Console.print]
    base = put(put(Vector.new(3, 0), 0, 5), 2, 7)
    left = put(base, 1, 1)
    right = put(put(base, 1, 2), 0, 9)
    leftAgain = put(left, 2, 8)
    Console.print("{at(left, 1)} {at(right, 1)} {at(base, 1)} {show(leftAgain)} {show(right)} {show(base)} {show(left)}")
"#,
        ),
        "1 2 0 5,1,8, 9,2,7, 5,0,7, 5,1,7,",
    );
}

#[test]
fn equality_between_versions_that_share_an_array() {
    assert_vm_and_wasm_gc(
        "vector-versions-eq",
        &program(
            r#"
fn main() -> Unit
    ! [Console.print]
    base = Vector.new(3, 0)
    a = put(base, 1, 4)
    b = put(base, 1, 4)
    c = put(a, 2, 1)
    back = put(c, 2, 0)
    Console.print("{a == b} {a == c} {back == a} {base == a} {base == base} {show(a)} {show(c)}")
"#,
        ),
        "true false true false true 0,4,0, 0,4,1,",
    );
}

#[test]
fn a_builder_loop_keeps_the_base_it_started_from() {
    assert_vm_and_wasm_gc(
        "vector-versions-builder",
        &program(
            r#"
fn fill(v: Vector<Int>, i: Int) -> Vector<Int>
    ? "Writes i * i at every index from i down to 0."
    match i < 0
        true -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i * i), v), i - 1)

fn main() -> Unit
    ! [Console.print]
    base = Vector.new(5, 7)
    built = fill(base, 4)
    again = fill(built, 1)
    Console.print("{show(base)} {show(built)} {show(again)} {show(fill(Vector.new(3, 1), 2))}")
"#,
        ),
        "7,7,7,7,7, 0,1,4,9,16, 0,1,4,9,16, 0,1,4,",
    );
}

#[test]
fn a_vector_in_a_record_the_caller_keeps_is_not_changed() {
    assert_vm_and_wasm_gc(
        "vector-versions-record",
        &program(
            r#"
record State
    cells: Vector<Int>
    count: Int

fn step(s: State, i: Int) -> State
    ? "Writes i at i; the None arm hands s back whole."
    match Vector.set(s.cells, i, i + 10)
        Option.Some(updated) -> State.update(s, cells = updated, count = s.count + 1)
        Option.None -> s

fn run(s: State, i: Int, n: Int) -> State
    ? "step for i up to n."
    match i >= n
        true -> s
        false -> run(step(s, i), i + 1, n)

fn main() -> Unit
    ! [Console.print]
    start = State(cells = Vector.new(4, 0), count = 0)
    half = run(start, 0, 2)
    done = run(half, 2, 6)
    Console.print("{show(start.cells)} {show(half.cells)} {show(done.cells)} {done.count} {half.count}")
"#,
        ),
        "0,0,0,0, 10,11,0,0, 10,11,12,13, 4 2",
    );
}

/// The measured fixture: 2000 sets of a 100,000-cell Vector held by a record.
#[test]
fn the_record_field_fixture_answers_as_the_vm_does() {
    let source = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/vector_field_set/main.av"
    ))
    .expect("read the fixture");
    assert_vm_and_wasm_gc("vector-versions-fixture", &source, "total 2000");
}

/// Nothing reads `b` after the second set, but `a` is described against
/// `b`, so that set must still record the cell it overwrites.
#[test]
fn a_dead_version_an_older_one_needs_is_not_written_over() {
    assert_vm_and_wasm_gc(
        "vector-versions-held",
        &program(
            r#"
fn main() -> Unit
    ! [Console.print]
    a = Vector.new(3, 0)
    c = match Vector.set(a, 0, 1)
        Option.Some(b) -> Option.withDefault(Vector.set(b, 1, 2), b)
        Option.None -> a
    Console.print("{show(a)} {show(c)}")
"#,
        ),
        "0,0,0, 1,2,0,",
    );
}

#[test]
fn a_vector_of_strings_and_a_miss_keep_their_versions() {
    assert_vm_and_wasm_gc(
        "vector-versions-strings",
        r#"
module Main
    intent = "Versions of a vector of strings, and a set past the end."
    effects [Console.print]

fn word(v: Vector<String>, i: Int) -> String
    ? "The cell, or ? outside the vector."
    Option.withDefault(Vector.get(v, i), "?")

fn main() -> Unit
    ! [Console.print]
    base = Vector.fromList(["a", "b", "c"])
    changed = Option.withDefault(Vector.set(base, 1, "x"), base)
    missed = Vector.set(changed, 3, "y")
    Console.print("{word(base, 1)} {word(changed, 1)} {word(base, 1)} {Vector.len(changed)} {missed == Option.None} {List.len(List.fromVector(base))}")
"#,
        "b x b 3 true 3",
    );
}

/// The fused `Option.withDefault(Vector.set(v, i, x), v)` and a recursive
/// helper that returns the vector it was given. Each vector some later line
/// still reads must come out unchanged when another binding is written
/// through the fusion, and the ones nothing reads again are written in place.
#[test]
fn the_self_keep_set_leaves_every_vector_still_read_unchanged() {
    assert_vm_and_wasm_gc(
        "vector-versions-self-keep",
        &program(
            r#"
fn fill(v: Vector<Int>, i: Int, n: Int) -> Vector<Int>
    ? "Writes i + 1 at every i below n."
    match i >= n
        true -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i + 1), v), i + 1, n)

fn same(v: Vector<Int>, k: Int) -> Vector<Int>
    ? "The vector itself, after k steps."
    match k == 0
        true -> v
        false -> same(v, k - 1)

fn sizeAt(v: Vector<Int>, i: Int) -> Vector<Int>
    ? "Writes the length at i; the value reads the vector itself."
    Option.withDefault(Vector.set(v, i, Vector.len(v)), v)

fn main() -> Unit
    ! [Console.print]
    base = Vector.new(4, 0)
    kept = same(base, 3)
    filled = fill(kept, 0, 3)
    once = Option.withDefault(Vector.set(filled, 3, 9), filled)
    outside = Option.withDefault(Vector.set(once, 7, 5), once)
    sized = sizeAt(outside, 0)
    fresh = fill(same(Vector.new(3, 0), 2), 0, 3)
    Console.print("{show(base)} {show(kept)} {show(filled)} {show(once)} {show(outside)} {show(sized)} {show(fresh)}")
"#,
        ),
        "0,0,0,0, 0,0,0,0, 1,2,3,0, 1,2,3,9, 1,2,3,9, 4,2,3,9, 1,2,3,",
    );
}

/// The sieve the speed scout measured: `mark` threads its vector through the
/// fused set in a self tail call, and `sieve` hands `mark` the vector and
/// keeps its result. Every call of the versioned `set` helper must pass the
/// owned flag, so the write records no old cell; it used to pass the constant
/// 0, because the fusion asked only the receiver read (never the last use)
/// and because the result of the recursive `mark` had no alias summary.
#[test]
fn a_vector_threaded_through_a_recursive_helper_is_written_in_place() {
    let source = r#"
module Main
    intent = "Sieve of Eratosthenes over a Vector."
    effects [Console.print]

fn mark(v: Vector<Bool>, j: Int, step: Int, n: Int) -> Vector<Bool>
    ? "Crosses out multiples."
    match j >= n
        true -> v
        false -> mark(Option.withDefault(Vector.set(v, j, false), v), j + step, step, n)

fn sieve(v: Vector<Bool>, i: Int, n: Int) -> Vector<Bool>
    ? "Sieves below n."
    match i * i >= n
        true -> v
        false -> match Option.withDefault(Vector.get(v, i), false)
            true -> sieve(mark(v, i * i, i, n), i + 1, n)
            false -> sieve(v, i + 1, n)

fn primes(v: Vector<Bool>, i: Int, acc: Int) -> Int
    ? "Counts the cells still true from i on."
    match Vector.get(v, i)
        Option.Some(true) -> primes(v, i + 1, acc + 1)
        Option.Some(false) -> primes(v, i + 1, acc)
        Option.None -> acc

"#;
    let fresh = format!(
        "{source}
fn main() -> Unit
    ! [Console.print]
    Console.print(\"{{primes(sieve(Vector.new(100, true), 2, 100), 2, 0)}}\")
"
    );
    // The same sieve on a vector main reads again afterwards: no write may
    // land in it.
    let kept = format!(
        "{source}
fn main() -> Unit
    ! [Console.print]
    base = Vector.new(100, true)
    v = sieve(base, 2, 100)
    Console.print(\"{{primes(v, 2, 0)}} {{primes(base, 2, 0)}}\")
"
    );
    let wat = wat_of(&fresh);
    let sets = func_bodies(&wat, " i32 i32 i32) (result (ref null ");
    assert_eq!(sets.len(), 1, "expected one Vector.set helper in:\n{wat}");
    let head = sets[0].lines().next().unwrap_or_default().trim();
    let name = head
        .strip_prefix("(func ")
        .and_then(|rest| rest.split(' ').next())
        .unwrap_or_else(|| panic!("helper name in {head:?}"));
    let call = match name.strip_prefix("(;").and_then(|n| n.strip_suffix(";)")) {
        Some(index) => format!("call {index}"),
        None => format!("call {name}"),
    };
    let lines: Vec<&str> = wat.lines().map(str::trim).collect();
    let flags: Vec<&str> = lines
        .windows(2)
        .filter(|pair| pair[1] == call)
        .map(|pair| pair[0])
        .collect();
    assert!(!flags.is_empty(), "no call of the set helper in:\n{wat}");
    assert!(
        flags.iter().all(|flag| *flag == "i32.const 1"),
        "a call of the set helper does not pass the owned flag: {flags:?}\n{wat}"
    );
    assert_vm_and_wasm_gc("vector-versions-sieve", &fresh, "25");
    assert_vm_and_wasm_gc("vector-versions-sieve-kept", &kept, "25 98");
}

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

/// The bodies of the functions in `wat` whose signature line contains
/// `signature`.
fn func_bodies<'w>(wat: &'w str, signature: &str) -> Vec<&'w str> {
    wat.match_indices("\n  (func ")
        .map(|(start, _)| {
            let body = &wat[start + 1..];
            &body[..body[1..]
                .find("\n  (func")
                .map_or(body.len(), |end| end + 1)]
        })
        .filter(|body| {
            body.lines()
                .next()
                .is_some_and(|line| line.contains(signature))
        })
        .collect()
}

/// A state record reaches `Vector.set` as a field of a record the function
/// was handed, which no ownership fact covers. The versioned `set` helper it
/// calls (the one function taking a version, an index, a `Bool` element and
/// the owned flag) must neither copy nor allocate an array.
#[test]
fn set_on_a_record_field_does_not_copy_the_array() {
    let wat = wat_of(
        r#"
module Main
    intent = "A state record whose vector every step sets."
    effects [Console.print]

record State
    cells: Vector<Bool>
    count: Int

fn step(s: State, i: Int) -> State
    ? "Sets cell i."
    match Vector.set(s.cells, i, true)
        Option.Some(updated) -> State.update(s, cells = updated, count = s.count + 1)
        Option.None -> s

verify step
    step(State(cells = Vector.fromList([false]), count = 0), 0).count => 1

fn main() -> Unit
    ! [Console.print]
    Console.print("{step(State(cells = Vector.fromList([false, false]), count = 0), 1).count}")
"#,
    );
    let sets = func_bodies(&wat, " i32 i32 i32) (result (ref null ");
    assert_eq!(sets.len(), 1, "expected one Vector.set helper in:\n{wat}");
    assert!(
        !sets[0].contains("array.copy") && !sets[0].contains("array.new"),
        "Vector.set copies or allocates an array:\n{}",
        sets[0]
    );
    assert!(
        sets[0].contains("array.set") && sets[0].contains("struct.new"),
        "Vector.set writes the cell and makes a new version:\n{}",
        sets[0]
    );
}

/// A program with lists, string interpolation and `+` but no Vector value.
/// The registry still keeps a `Vector<T>` array for every `List<T>` and the
/// concatenation helper's `Vector<String>`, but they stay plain arrays with
/// no version or diff struct and no `current` / `set` helper, so the module
/// is byte for byte what it was before vectors had versions. The hash is the
/// one the compiler produced before them; a change to codegen that moves it
/// for another reason updates it, one that moves it because of versions is
/// the regression this pins.
#[test]
fn a_program_without_a_vector_value_is_unchanged_by_versions() {
    use sha2::{Digest, Sha256};
    let source = r#"module Main
    intent = "Lists, string interpolation and concatenation, and no Vector."
    effects [Console.print]

fn total(xs: List<Int>, acc: Int) -> Int
    ? "The sum of the list."
    match xs
        [] -> acc
        [x, ..rest] -> total(rest, acc + x)

fn words(xs: List<String>, acc: String) -> String
    ? "The words, joined."
    match xs
        [] -> acc
        [w, ..rest] -> words(rest, acc + w + " ")

fn main() -> Unit
    ! [Console.print]
    xs = [1, 2, 3]
    Console.print("total {total(xs, 0)} of {List.len(xs)}: {words(["a", "b"], "")}")
"#;
    assert_vm_and_wasm_gc("vector-versions-none-answer", source, "total 6 of 3: a b");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module("vector-versions-none", source);
    let out_dir = path.with_extension("out");
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .args(["--target", "wasm-gc", "-o"])
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile` to execute");
    assert!(
        out.status.success(),
        "compile failed:\n{}",
        format_output(&out)
    );
    let wasm = std::fs::read_dir(&out_dir)
        .expect("output dir")
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .find(|p| p.extension().is_some_and(|ext| ext == "wasm"))
        .expect("a .wasm in the output");
    let bytes = std::fs::read(&wasm).expect("read the module");
    cleanup(&path);
    let _ = std::fs::remove_dir_all(&out_dir);
    let digest: String = Sha256::digest(&bytes)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect();
    // Self-tail loops changed `total` and `words`. With that lowering off,
    // this fixture still produces the preceding pinned bytes; versions add
    // no vector layout here. Keep pinning the current default output too
    // (which exports only `_start`, `main` and the host helpers, and since
    // the emitter drops the functions nothing reaches, carries no unused
    // helper either). `words` also holds its `String` accumulator as a
    // buffer (`body/from_mir/str_builder.rs`), its string literals are
    // shared through globals (`body/from_mir/strings.rs`), and its twin loops
    // only while its Int params are words (`self_tail_loop.rs`), and its list
    // cells carry their length (`lists::emit_list_len`); none is a vector
    // layout.
    assert_eq!(
        (bytes.len(), digest.as_str()),
        (
            4629,
            "186155c4853f404788bd486b11ba2db9111e24abb7be211a60022de769415258"
        ),
        "a module with no Vector value changed"
    );
}

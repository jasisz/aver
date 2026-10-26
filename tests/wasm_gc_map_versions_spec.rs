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

/// `{}` is one map per `Map<K, V>`, shared by every use and never written:
/// an insert into it grows into arrays of its own. Every read of it answers
/// empty, and maps made from it, even two from one use, stay apart.
#[test]
fn the_shared_empty_map_reads_as_empty_and_stays_empty() {
    assert_vm_and_wasm_gc(
        "map-versions-empty",
        r#"
module Main
    intent = "The empty map reads as empty, and maps made from it stay apart."
    effects [Console.print]

record Box
    m: Map<String, Int>
    n: Int

fn none() -> Map<String, Int>
    ? "The empty map."
    {}

fn emptied() -> Map<String, Int>
    ? "A map that had an entry and lost it."
    Map.remove(Map.set({}, "q", 5), "q")

fn fill(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    ? "Adds keys k0..k(n-1)."
    match i >= n
        true -> m
        false -> fill(Map.set(m, "k{i}", i), i + 1, n)

fn lookup(m: Map<String, Int>, k: String) -> String
    ? "What a match on the lookup sees."
    match Map.get(m, k)
        Option.Some(v) -> "some {v}"
        Option.None -> "none"

fn main() -> Unit
    ! [Console.print]
    e = none()
    Console.print("{Map.len(e)} {Map.has(e, "a")} {Option.withDefault(Map.get(e, "a"), 7)} {lookup(e, "a")} {List.len(Map.keys(e))} {List.len(Map.values(e))} {List.len(Map.entries(e))} {Map.len(Map.remove(e, "a"))}")
    a = Map.set(e, "x", 1)
    b = Map.set(e, "y", 2)
    c = Map.set(Map.set(e, "x", 3), "z", 4)
    Console.print("{Map.len(e)} {Map.has(e, "x")} {lookup(a, "x")} {Map.has(a, "y")} {lookup(b, "y")} {Map.has(b, "x")} {Map.len(c)} {lookup(c, "x")} {lookup(a, "x")}")
    Console.print("{e == none()} {e == emptied()} {emptied() == e} {a == b} {Map.set(e, "x", 1) == a} {Map.remove(a, "x") == e} {Map.remove(a, "x") == none()}")
    big = fill(e, 0, 100)
    other = fill(none(), 50, 120)
    Console.print("{Map.len(big)} {Map.len(other)} {lookup(big, "k99")} {lookup(other, "k49")} {lookup(other, "k119")} {Map.len(e)} {big == fill({}, 0, 100)}")
    box = Box(m = {}, n = 1)
    moved = Box(m = Map.set(box.m, "x", 1), n = 2)
    Console.print("{box == Box(m = none(), n = 1)} {Box(m = emptied(), n = 1) == box} {Box(m = a, n = 1) == box} {Map.len(box.m)} {lookup(moved.m, "x")} {Map.len(Map.fromList([]))} {Map.fromList([("x", 1)]) == a}")
"#,
        "0 false 7 none 0 0 0 0\n\
         0 false some 1 false some 2 false 2 some 3 some 1\n\
         true true true false true true true\n\
         100 70 some 99 none some 119 0 true\n\
         true true false 0 some 1 0 true",
    );
}

/// A map takes a bucket from the low bits of a key's hash, and a Small
/// `Int` used to hash as `s ^ (s >>> 32)`: the keys 0, 1, 2, … filled one
/// run of neighbouring buckets, `-1 - k` shared `k`'s hash, and multiples
/// of a power of two shared a bucket. `__aint_hash` mixes the word now.
/// Iteration is sorted by key, so nothing a program prints may change: this
/// runs keys close together, below zero, spaced by 4096, at both ends of a
/// machine word and past it, as record fields, and behind a bounded carrier,
/// through `set`, `get`, `has`, `remove`, `keys`, `values`, `entries` and `==`.
#[test]
fn int_keys_read_the_same_however_they_are_hashed() {
    assert_vm_and_wasm_gc(
        "map-int-keys",
        r#"
module Main
    intent = "Int keys that sit close together, below zero, at the ends of a machine word and past it, in records and behind a bounded carrier."
    exposes opaque [Slot]
    effects [Console.print]

record Cell
    x: Int
    y: Int

fn fill(m: Map<Int, Int>, from: Int, to: Int, step: Int) -> Map<Int, Int>
    ? "Maps from, from + step, ... below to, each to twice itself."
    match from >= to
        true -> m
        false -> fill(Map.set(m, from, from * 2), from + step, to, step)

fn drop(m: Map<Int, Int>, from: Int, to: Int, step: Int) -> Map<Int, Int>
    ? "Removes from, from + step, ... below to."
    match from >= to
        true -> m
        false -> drop(Map.remove(m, from), from + step, to, step)

fn found(m: Map<Int, Int>, from: Int, to: Int, acc: Int) -> Int
    ? "Sums the values of from..to-1, counting -1 for each absent key."
    match from >= to
        true -> acc
        false -> found(m, from + 1, to, acc + Option.withDefault(Map.get(m, from), 0 - 1))

fn at(m: Map<Int, Int>, k: Int) -> Int
    ? "The value at k, -1 when absent."
    Option.withDefault(Map.get(m, k), 0 - 1)

fn total(ks: List<Int>, acc: Int) -> Int
    ? "The sum of a list."
    match ks
        [] -> acc
        [k, ..rest] -> total(rest, acc + k)

fn pairs(es: List<Tuple<Int, Int>>, acc: Int) -> Int
    ? "Folds the entries in order: acc * 7 + key * 3 + value."
    match es
        [] -> acc
        [e, ..rest] -> match e
            (k, v) -> pairs(rest, acc * 7 + k * 3 + v)

fn grid(m: Map<Cell, Int>, x: Int, y: Int, n: Int) -> Map<Cell, Int>
    ? "Maps every cell of an n by n grid to x * n + y."
    match x >= n
        true -> m
        false -> match y >= n
            true -> grid(m, x + 1, 0, n)
            false -> grid(Map.set(m, Cell(x = x, y = y), x * n + y), x, y + 1, n)

fn cells(m: Map<Cell, Int>, x: Int, n: Int, acc: Int) -> Int
    ? "Sums the diagonal cells and one cell off the grid per row."
    match x >= n
        true -> acc
        false -> cells(m, x + 1, n, acc + Option.withDefault(Map.get(m, Cell(x = x, y = x)), 0) + Option.withDefault(Map.get(m, Cell(x = x, y = n)), 1000))

record Slot
    value: Int

record Place
    slot: Slot
    row: Int

fn slotOf(n: Int) -> Result<Slot, String>
    ? "A slot between 0 and 5000."
    match Bool.and(n >= 0, n <= 5000)
        true -> Result.Ok(Slot(value = n))
        false -> Result.Err("out of range")

fn slot(n: Int) -> Slot
    ? "The slot n, or slot 0."
    match slotOf(n)
        Result.Ok(s) -> s
        Result.Err(_) -> Slot(value = 0)

fn fillSlots(m: Map<Slot, Int>, i: Int, n: Int) -> Map<Slot, Int>
    ? "Maps slots i..n-1 to their square."
    match i >= n
        true -> m
        false -> fillSlots(Map.set(m, slot(i), i * i), i + 1, n)

fn places(m: Map<Place, Int>, i: Int, n: Int) -> Map<Place, Int>
    ? "Maps the places (i, i mod 7) for i below n to i."
    match i >= n
        true -> m
        false -> places(Map.set(m, Place(slot = slot(i), row = Int.mod(i, 7)), i), i + 1, n)

fn dropSlots(m: Map<Slot, Int>, i: Int, n: Int) -> Map<Slot, Int>
    ? "Removes every second slot from i below n."
    match i >= n
        true -> m
        false -> dropSlots(Map.remove(m, slot(i)), i + 2, n)

fn main() -> Unit
    ! [Console.print]
    dense = fill({}, 0, 3000, 1)
    thinned = drop(dense, 0, 3000, 3)
    Console.print("{Map.len(dense)} {found(dense, 2990, 3010, 0)} {Map.len(thinned)} {found(thinned, 0, 9, 0)} {Map.has(thinned, 2999)} {Map.has(thinned, 2997)}")
    Console.print("{total(Map.keys(thinned), 0)} {total(Map.values(thinned), 0)} {pairs(Map.entries(drop(dense, 0, 2990, 1)), 0)}")
    signed = fill({}, 0 - 1500, 1500, 1)
    Console.print("{Map.len(signed)} {at(signed, 0 - 1)} {at(signed, 0)} {found(signed, 0 - 1502, 0 - 1497, 0)} {total(Map.keys(drop(signed, 0 - 1500, 1490, 1)), 0)}")
    strided = fill({}, 0, 4096 * 1024, 4096)
    Console.print("{Map.len(strided)} {Map.has(strided, 4096 * 1023)} {Map.has(strided, 4096 * 1023 + 1)} {Map.len(drop(strided, 0, 4096 * 1024, 8192))}")
    top = 9223372036854775807
    bottom = 0 - 9223372036854775807 - 1
    ends = Map.set(Map.set(Map.set(Map.set(Map.set({}, top, 1), bottom, 2), top + 1, 3), bottom - 1, 4), top * top, 5)
    Console.print("{Map.len(ends)} {at(ends, 9223372036854775807)} {at(ends, 0 - 9223372036854775808)} {at(ends, 9223372036854775808)} {at(ends, 0 - 9223372036854775809)} {at(ends, top * top)} {at(ends, top - 1)} {List.len(Map.keys(Map.remove(ends, top + 1)))} {total(Map.keys(ends), 0) == top + bottom + top + 1 + bottom - 1 + top * top}")
    a = fill({}, 0, 500, 1)
    b = drop(fill(fill({}, 250, 600, 1), 0, 250, 1), 500, 600, 1)
    Console.print("{a == b} {a == Map.set(b, 7, 0)} {Map.remove(a, 499) == Map.remove(b, 499)} {Map.set(a, top + 1, 0) == Map.set(b, top + 1, 0)} {Map.set(a, top + 1, 0) == Map.set(b, top + 2, 0)}")
    g = grid({}, 0, 0, 60)
    Console.print("{Map.len(g)} {cells(g, 0, 60, 0)} {Option.withDefault(Map.get(g, Cell(x = 59, y = 0)), 0 - 1)} {Map.len(Map.remove(g, Cell(x = 3, y = 4)))}")
    m = fillSlots({}, 0, 4000)
    d = dropSlots(m, 0, 4000)
    p = places({}, 0, 3000)
    Console.print("{Map.len(m)} {Option.withDefault(Map.get(m, slot(3999)), 0 - 1)} {Map.len(d)} {Map.has(d, slot(10))} {Map.has(d, slot(11))} {Option.withDefault(Map.get(d, slot(4001)), 0 - 1)}")
    Console.print("{Map.len(p)} {Option.withDefault(Map.get(p, Place(slot = slot(2999), row = 3)), 0 - 1)} {Option.withDefault(Map.get(p, Place(slot = slot(2999), row = 4)), 0 - 1)} {Map.len(Map.remove(p, Place(slot = slot(5), row = 5)))}")
"#,
        "3000 59880 2000 51 true false\n\
         3000000 6000000 703873392265\n\
         3000 -2 0 -8996 14945\n\
         1024 true false 512\n\
         5 1 2 3 4 5 -1 4 true\n\
         true false true true false\n\
         3600 167970 3540 3599\n\
         4000 15992001 2000 false true -1\n\
         3000 2999 -1 2999",
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

/// A map gets four buckets on its first insert, sixteen when a fourth
/// entry arrives, and doubles from there at twelve and twenty-four
/// entries. At each size on either side of those steps this makes two inserts and an overwrite from one version, which
/// grow out of it when they cross a step and write into its arrays when
/// they do not, removes from one of them, and reads them all afterwards
/// through `get`, `keys`, `values` and `entries`. The same entries built
/// top down, and kept by removing every larger key from a map of thirty,
/// sit in tables of other sizes and other layouts: they must be equal,
/// on their own, as fields of a record and as items of a list.
#[test]
fn small_maps_read_the_same_across_every_early_doubling() {
    assert_vm_and_wasm_gc(
        "map-versions-small",
        r#"
module Main
    intent = "Maps of one to twenty-five entries on either side of each early doubling, read every way after writes from one version."
    effects [Console.print]

record Box
    m: Map<Int, Int>
    n: Int

fn fill(m: Map<Int, Int>, from: Int, to: Int) -> Map<Int, Int>
    ? "Maps each key in from..to-1 to ten times itself."
    match from >= to
        true -> m
        false -> fill(Map.set(m, from, from * 10), from + 1, to)

fn fillDown(m: Map<Int, Int>, from: Int, to: Int) -> Map<Int, Int>
    ? "The same entries as fill, inserted from the top down."
    match to <= from
        true -> m
        false -> fillDown(Map.set(m, to - 1, (to - 1) * 10), from, to - 1)

fn dropRange(m: Map<Int, Int>, from: Int, to: Int) -> Map<Int, Int>
    ? "Removes keys from..to-1."
    match from >= to
        true -> m
        false -> dropRange(Map.remove(m, from), from + 1, to)

fn total(m: Map<Int, Int>, keys: List<Int>, acc: Int) -> Int
    ? "Sums key * 1000 + value over the listed keys, and counts an absent one as -1."
    match keys
        [] -> acc
        [k, ..rest] -> match Map.get(m, k)
            Option.Some(v) -> total(m, rest, acc + k * 1000 + v)
            Option.None -> total(m, rest, acc - 1)

fn firstKeys(keys: List<Int>, acc: String) -> String
    ? "The keys, comma-separated."
    match keys
        [] -> acc
        [k, ..rest] -> firstKeys(rest, "{acc},{k}")

fn summary(m: Map<Int, Int>) -> String
    ? "Size, a checksum of the entries, and the first keys in order."
    "{Map.len(m)}:{total(m, Map.keys(m), 0)}:{firstKeys(List.take(Map.keys(m), 4), "")}"

fn entrySum(entries: List<Tuple<Int, Int>>, acc: Int) -> Int
    ? "Sums key + value over the entries."
    match entries
        [] -> acc
        [e, ..rest] -> match e
            (k, v) -> entrySum(rest, acc + k + v)

fn valueSum(values: List<Int>, acc: Int) -> Int
    ? "Sums the values."
    match values
        [] -> acc
        [v, ..rest] -> valueSum(rest, acc + v)

fn step(n: Int) -> String
    ? "A map of n entries, two inserts, an overwrite and a remove made from its versions, and the same entries built two other ways."
    m = fill({}, 0, n)
    a = Map.set(m, n, 1)
    b = Map.set(m, n + 1, 2)
    c = Map.set(m, 0, 5)
    r = Map.remove(a, 0)
    down = fillDown({}, 0, n)
    shrunk = dropRange(fill({}, 0, 30), n, 30)
    "{summary(m)} {summary(a)} {summary(b)} {summary(c)} {summary(r)} {entrySum(Map.entries(r), 0)} {valueSum(Map.values(c), 0)} {m == down} {m == shrunk} {a == b} {Box(m = down, n = n) == Box(m = shrunk, n = n)} {[m, down] == [shrunk, m]}"

fn main() -> Unit
    ! [Console.print]
    Console.print(step(1))
    Console.print(step(2))
    Console.print(step(3))
    Console.print(step(4))
    Console.print(step(5))
    Console.print(step(6))
    Console.print(step(7))
    Console.print(step(12))
    Console.print(step(13))
    Console.print(step(24))
    Console.print(step(25))
"#,
        "1:0:,0 2:1001:,0,1 2:2002:,0,2 1:5:,0 1:1001:,1 2 5 true true false true true\n\
         2:1010:,0,1 3:3011:,0,1,2 3:4012:,0,1,3 2:1015:,0,1 2:3011:,1,2 14 15 true true false true true\n\
         3:3030:,0,1,2 4:6031:,0,1,2,3 4:7032:,0,1,2,4 3:3035:,0,1,2 3:6031:,1,2,3 37 35 true true false true true\n\
         4:6060:,0,1,2,3 5:10061:,0,1,2,3 5:11062:,0,1,2,3 4:6065:,0,1,2,3 4:10061:,1,2,3,4 71 65 true true false true true\n\
         5:10100:,0,1,2,3 6:15101:,0,1,2,3 6:16102:,0,1,2,3 5:10105:,0,1,2,3 5:15101:,1,2,3,4 116 105 true true false true true\n\
         6:15150:,0,1,2,3 7:21151:,0,1,2,3 7:22152:,0,1,2,3 6:15155:,0,1,2,3 6:21151:,1,2,3,4 172 155 true true false true true\n\
         7:21210:,0,1,2,3 8:28211:,0,1,2,3 8:29212:,0,1,2,3 7:21215:,0,1,2,3 7:28211:,1,2,3,4 239 215 true true false true true\n\
         12:66660:,0,1,2,3 13:78661:,0,1,2,3 13:79662:,0,1,2,3 12:66665:,0,1,2,3 12:78661:,1,2,3,4 739 665 true true false true true\n\
         13:78780:,0,1,2,3 14:91781:,0,1,2,3 14:92782:,0,1,2,3 13:78785:,0,1,2,3 13:91781:,1,2,3,4 872 785 true true false true true\n\
         24:278760:,0,1,2,3 25:302761:,0,1,2,3 25:303762:,0,1,2,3 24:278765:,0,1,2,3 24:302761:,1,2,3,4 3061 2765 true true false true true\n\
         25:303000:,0,1,2,3 26:328001:,0,1,2,3 26:329002:,0,1,2,3 25:303005:,0,1,2,3 25:328001:,1,2,3,4 3326 3005 true true false true true",
    );
}

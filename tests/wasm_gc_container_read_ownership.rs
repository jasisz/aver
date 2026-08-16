//! A value read out of a container is not yours to mutate (#950).
//!
//! On the wasm-gc backend the in-place `Vector.set` / `Map.set` fast
//! path is granted statically, from `last_use` + `aliased_slots`. Two
//! holes let a container-read value take that fast path and write
//! straight through to the stored entry:
//!
//! - every `aver run --wasm-gc` / `--wasip2` / `compile` path re-ran
//!   the slot resolver after `flatten_multimodule`, and a fresh
//!   `FnResolution` carries the all-`false` default `aliased_slots` —
//!   the re-resolve silently withdrew every verdict `ir::alias` had
//!   stamped, while the stale `last_use` marks survived. A local
//!   extracted from a Map entry then read as "dead and never shared";
//! - `mir_arg_uniquely_owned` answered `true` for every NON-local
//!   receiver, so an inline `Vector.set(Option.withDefault(
//!   Map.get(m, k), d), i, x)` mutated the map-held array with no
//!   local involved at all.
//!
//! The probe answered 5011/1003-style write-through values where
//! compiled Rust and the self-host interpreter answered 7011/7003.
//!
//! This file pins the full provenance × operation matrix, every cell
//! run on VM, wasm-gc, and self-host through the real CLI, each backend
//! asserted against the same hand-computed literal — three backends
//! agreeing on a wrong value still fails. The two green-by-design cells
//! (fresh local, fresh chain) pin that the conservative fix did not
//! swallow the owned fast path, and `fresh_local_set_stays_in_place`
//! additionally pins the *emitted code*: the clone (`array.new_default`)
//! exists only in the container-read variant.

#![cfg(feature = "wasm")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{}-{}", prefix, nanos));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &std::path::Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// Run one cell program through the real CLI on the given backend
/// flags, returning trimmed stdout.
fn run_cli(prefix: &str, source: &str, extra_args: &[&str]) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root).arg("run").arg(&path);
    for a in extra_args {
        if *a == "--module-root" {
            cmd.arg(a)
                .arg(path.parent().expect("temp module has parent"));
        } else {
            cmd.arg(a);
        }
    }
    let out = cmd.output().expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{} run {:?} failed:\n{}",
        prefix,
        extra_args,
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Assert one matrix cell: VM, wasm-gc and self-host each print exactly
/// the hand-computed literal.
fn assert_cell(name: &str, source: &str, expected: &str) {
    let vm = run_cli(name, source, &[]);
    assert_eq!(
        vm, expected,
        "{name}: VM diverged from the hand-computed answer"
    );
    let wasm = run_cli(name, source, &["--wasm-gc"]);
    assert_eq!(
        wasm, expected,
        "{name}: wasm-gc diverged from the hand-computed answer — an \
         in-place mutation reached a container-held value"
    );
    let self_host = run_cli(name, source, &["--module-root", "--self-host"]);
    assert_eq!(
        self_host, expected,
        "{name}: self-host diverged from the hand-computed answer"
    );
}

/// Every write-through cell prints `x + 5000` for x ∈ {2011, 2003}: the
/// stored entry must still hold `x` after the local was poked to 5000.
/// A backend that writes through prints 10000 (5000 + 5000) instead.
const POKED: &str = "7011\n7003";

#[test]
fn map_entry_local_vector_set_does_not_write_through() {
    assert_cell(
        "own-c01",
        r#"
fn probe(x: Int) -> Int
    ? "Map entry read into a local, Vector.set on the local, entry re-read."
    stash = {"k" => Vector.fromList([x, x + 2])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn map_entry_local_fused_self_keep_set_does_not_write_through() {
    assert_cell(
        "own-c02",
        r#"
fn probe(x: Int) -> Int
    ? "Map entry read into a local, fused self-keep Vector.set, entry re-read."
    stash = {"k" => Vector.fromList([x, x + 2])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = Option.withDefault(Vector.set(held, 0, 5000), held)
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn nested_map_entry_local_map_set_does_not_write_through() {
    assert_cell(
        "own-c03",
        r#"
fn probe(x: Int) -> Int
    ? "Inner map read out of a Map-in-Map, Map.set on the local, outer re-read."
    outer = {"in" => {"a" => x}}
    inner = Option.withDefault(Map.get(outer, "in"), {})
    poked = Map.set(inner, "a", 5000)
    back = Option.withDefault(Map.get(outer, "in"), {})
    stored = Option.withDefault(Map.get(back, "a"), 0 - 1)
    fresh = Option.withDefault(Map.get(poked, "a"), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn list_element_local_vector_set_does_not_write_through() {
    assert_cell(
        "own-c04",
        r#"
fn probe(x: Int) -> Int
    ? "List element read into a local by pattern match, Vector.set, element re-read."
    lst = [Vector.fromList([x, 9])]
    held = match lst
        [] -> Vector.fromList([0, 0])
        [h, ..t] -> h
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    again = match lst
        [] -> Vector.fromList([0, 0])
        [h2, ..t2] -> h2
    stored = Option.withDefault(Vector.get(again, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn helper_returned_container_read_vector_set_does_not_write_through() {
    assert_cell(
        "own-c05",
        r#"
fn fetch(m: Map<String, Vector<Int>>) -> Vector<Int>
    ? "Read the k entry out of the map."
    Option.withDefault(Map.get(m, "k"), Vector.fromList([]))

fn probe(x: Int) -> Int
    ? "Helper returns a map-held vector, Vector.set on it, entry re-read."
    stash = {"k" => Vector.fromList([x, 9])}
    held = fetch(stash)
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn vector_param_set_does_not_clobber_the_callers_local() {
    assert_cell(
        "own-c06",
        r#"
fn stamp(v: Vector<Int>) -> Vector<Int>
    ? "Vector.set on a parameter the caller still holds."
    Option.withDefault(Vector.set(v, 0, 5000), Vector.fromList([]))

fn probe(x: Int) -> Int
    ? "Pass a local to a setter, then read the local again."
    mine = Vector.fromList([x, 9])
    poked = stamp(mine)
    kept = Option.withDefault(Vector.get(mine, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    kept + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn map_param_set_does_not_clobber_the_callers_local() {
    assert_cell(
        "own-c07",
        r#"
fn put(m: Map<String, Int>) -> Map<String, Int>
    ? "Map.set on a parameter the caller still holds."
    Map.set(m, "a", 5000)

fn probe(x: Int) -> Int
    ? "Pass a local map to a setter, then read the local again."
    mine = {"a" => x}
    poked = put(mine)
    kept = Option.withDefault(Map.get(mine, "a"), 0 - 1)
    fresh = Option.withDefault(Map.get(poked, "a"), 0 - 1)
    kept + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn fresh_local_set_still_answers_right() {
    assert_cell(
        "own-c08",
        r#"
fn probe(x: Int) -> Int
    ? "Freshly built vector, dead after the set: the in-place fast path is fine."
    fresh = Vector.fromList([x, 9])
    poked = Option.withDefault(Vector.set(fresh, 0, x + 5000), Vector.fromList([]))
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    a + b

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "7020\n7012",
    );
}

#[test]
fn inline_container_read_vector_set_does_not_write_through() {
    assert_cell(
        "own-c09",
        r#"
fn probe(x: Int) -> Int
    ? "Vector.set straight on an inline Map.get read, no local in between."
    stash = {"k" => Vector.fromList([x, 9])}
    poked = Option.withDefault(Vector.set(Option.withDefault(Map.get(stash, "k"), Vector.fromList([])), 0, 5000), Vector.fromList([]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn inline_container_read_map_set_does_not_write_through() {
    assert_cell(
        "own-c10",
        r#"
fn probe(x: Int) -> Int
    ? "Map.set straight on an inline Map.get read of a nested map."
    outer = {"in" => {"a" => x}}
    poked = Map.set(Option.withDefault(Map.get(outer, "in"), {}), "a", 5000)
    back = Option.withDefault(Map.get(outer, "in"), {})
    stored = Option.withDefault(Map.get(back, "a"), 0 - 1)
    fresh = Option.withDefault(Map.get(poked, "a"), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn fresh_chained_map_set_still_answers_right() {
    assert_cell(
        "own-c11",
        r#"
fn probe(x: Int) -> Int
    ? "Chained Map.set over a fresh map: the owned fast path must survive."
    base = Map.set({}, "a", x)
    two = Map.set(Map.set(base, "b", 2), "c", 3)
    got = Option.withDefault(Map.get(two, "a"), 0 - 1)
    got + Map.len(two)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "2014\n2006",
    );
}

// ── Match-binder provenance (#953 follow-up) ──────────────────────
//
// A match arm's pattern binders are container reads too: the binder
// aliases (part of) the subject, and the subject may be a stored
// entry. The alias pass used to flag only `Stmt::Binding` slots, so
// every binder slot read as "never shared" — wasm-gc mutated the
// stored entry in place (10000/10000), and the VM's static owned
// mask took the arena entry with `mem::take` while the container
// still held it (4999/4999, silent). Each cell below is one binder
// shape; all three backends must answer the hand-computed literal.

#[test]
fn bare_ident_binder_vector_set_does_not_write_through() {
    // This shape used to TRAP on wasm-gc (`unreachable`): the MIR
    // emitter had no arm for a single-Bind match over a collection
    // subject, so `probe` compiled to a trap stub.
    assert_cell(
        "own-c12",
        r#"
fn probe(x: Int) -> Int
    ? "Launder a flagged container-read local through a bare Ident match binder, mutate the binder."
    stash = {"k" => Vector.fromList([x, x + 2])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = match held
        v -> Option.withDefault(Vector.set(v, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn some_binder_vector_set_does_not_write_through() {
    assert_cell(
        "own-c13",
        r#"
fn probe(x: Int) -> Int
    ? "Mutate the Some-binder of a map read inside the arm body."
    stash = {"k" => Vector.fromList([x, x + 2])}
    poked = match Map.get(stash, "k")
        Option.None -> Vector.fromList([0, 0])
        Option.Some(v) -> Option.withDefault(Vector.set(v, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn cons_head_binder_vector_set_does_not_write_through() {
    assert_cell(
        "own-c14",
        r#"
fn probe(x: Int) -> Int
    ? "Mutate the head binder of a list pattern inside the arm body."
    lst = [Vector.fromList([x, 9])]
    poked = match lst
        [] -> Vector.fromList([0, 0])
        [h, ..t] -> Option.withDefault(Vector.set(h, 0, 5000), Vector.fromList([0, 0]))
    again = match lst
        [] -> Vector.fromList([0, 0])
        [h2, ..t2] -> h2
    stored = Option.withDefault(Vector.get(again, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn tuple_binder_vector_set_does_not_write_through() {
    assert_cell(
        "own-c15",
        r#"
fn probe(x: Int) -> Int
    ? "Mutate a tuple-pattern binder that aliases the tuple's element."
    pair = (Vector.fromList([x, 9]), 1)
    poked = match pair
        (v, n) -> Option.withDefault(Vector.set(v, 0, 5000), Vector.fromList([0, 0]))
    kept = match pair
        (v2, n2) -> v2
    stored = Option.withDefault(Vector.get(kept, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn some_binder_map_set_does_not_write_through() {
    assert_cell(
        "own-c16",
        r#"
fn probe(x: Int) -> Int
    ? "Map.set on the Some-binder of a nested-map read inside the arm."
    outer = {"in" => {"a" => x}}
    poked = match Map.get(outer, "in")
        Option.None -> {"a" => 0 - 1}
        Option.Some(m) -> Map.set(m, "a", 5000)
    back = Option.withDefault(Map.get(outer, "in"), {"a" => 0 - 1})
    stored = Option.withDefault(Map.get(back, "a"), 0 - 1)
    fresh = Option.withDefault(Map.get(poked, "a"), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

// ── A FRESH subject still RETAINS what was put in it ──────────────
//
// The row above reads a value OUT of a container. This one puts a
// value IN, spells the aggregate as a match subject, and pokes the
// local afterwards. The escape half used to skip a match's subject
// entirely — the arm tails become the value, so nothing of the subject
// escapes through it — and the binder pass ran that half over the
// subject only when the subject was NOT provably fresh. Both halves of
// that were the same mistake: freshness is a claim about the
// AGGREGATE, never about its contents, so `held` stayed owned-eligible
// and the poke wrote into the map the binder was still holding. Every
// cell here answered 10000 on wasm-gc (write-through: 5000 read back
// where `x` was stored, plus the 5000 poked in), and the nested-map
// cell tripped the VM's own debug audit.

#[test]
fn map_literal_match_subject_retains_the_local_it_holds() {
    assert_cell(
        "own-c17",
        r#"
fn probe(x: Int) -> Int
    ? "Fresh map-literal subject retains held; the binder keeps a handle, then held is poked."
    held = Vector.fromList([x, x + 2])
    keeper = match {"k" => held}
        mm -> mm
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(keeper, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn from_list_match_subject_retains_its_element() {
    assert_cell(
        "own-c18",
        r#"
fn probe(x: Int) -> Int
    ? "Fresh Vector.fromList subject retains held through its element; held is then poked."
    held = Vector.fromList([x, x + 2])
    carrier = match Vector.fromList([held])
        c -> c
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    inner = Option.withDefault(Vector.get(carrier, 0), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(inner, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn map_set_match_subject_retains_its_value() {
    assert_cell(
        "own-c19",
        r#"
fn probe(x: Int) -> Int
    ? "Fresh Map.set subject retains held as its value; held is then poked."
    held = Vector.fromList([x, x + 2])
    keeper = match Map.set({}, "k", held)
        mm -> mm
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(keeper, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

#[test]
fn nested_map_literal_match_subject_retains_the_inner_map() {
    assert_cell(
        "own-c20",
        r#"
fn probe(x: Int) -> Int
    ? "Map spelling: fresh map-literal subject retains inner; inner is then Map.set."
    inner = Map.set({}, "a", x)
    keeper = match {"in" => inner}
        mm -> mm
    poked = Map.set(inner, "a", 5000)
    back = Option.withDefault(Map.get(keeper, "in"), {})
    stored = Option.withDefault(Map.get(back, "a"), 0 - 1)
    fresh = Option.withDefault(Map.get(poked, "a"), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

/// The same retention, poked through the FUSED self-keep set — the
/// `VECTOR_SET_OR_KEEP` opcode, which is the VM's only true in-place
/// arena write. Its static grant used to reach the arena with neither
/// the runtime fence nor the debug audit in front of it, so the VM
/// printed 10000 here while wasm-gc printed the right answer: the one
/// cell in this file whose original failure was the VM's alone.
#[test]
fn map_literal_match_subject_retention_survives_the_fused_self_keep_set() {
    assert_cell(
        "own-c21",
        r#"
fn probe(x: Int) -> Int
    ? "Fresh map-literal subject retains held; held is poked through the fused self-keep set."
    held = Vector.fromList([x, x + 2])
    keeper = match {"k" => held}
        mm -> mm
    poked = Option.withDefault(Vector.set(held, 0, 5000), held)
    back = Option.withDefault(Map.get(keeper, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

// ── A fresh receiver is EARNED, not free ──────────────────────────
//
// The owned wasm-gc `Vector.set` emitted its receiver three times —
// once for the bounds check, once as the `array.set` target, once as
// the `Some` payload. Free for a local; for the other owned-eligible
// receiver, a provably-fresh non-local, each emission BUILT ANOTHER
// ARRAY, so the write landed on build #2 and the answer came back out
// of the untouched build #3.

/// `x + 5000` written into a fresh receiver, read straight back:
/// 7011/7003 everywhere, and 2011/2003 on wasm-gc when the write goes
/// to an array nobody reads.
#[test]
fn inline_fresh_receiver_set_is_read_back_from_the_array_it_wrote() {
    assert_cell(
        "own-c22",
        r#"
fn probe(x: Int) -> Int
    ? "Vector.set straight on an inline fresh Vector.fromList receiver."
    poked = Option.withDefault(Vector.set(Vector.fromList([x, 9]), 0, x + 5000), Vector.fromList([]))
    Option.withDefault(Vector.get(poked, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        POKED,
    );
}

/// Chained: the receiver is itself an owned `Vector.set` result. Both
/// writes have to land in the one array the result names — cell 1 keeps
/// the 7 the inner set wrote, cell 0 the `x + 5000` the outer one did,
/// so the sum moves if either build is duplicated.
#[test]
fn chained_fresh_receiver_sets_land_in_one_array() {
    assert_cell(
        "own-c23",
        r#"
fn probe(x: Int) -> Int
    ? "Chained fresh receiver: Vector.set of a Vector.set result, one build only."
    poked = Option.withDefault(Vector.set(Option.withDefault(Vector.set(Vector.fromList([x, 9]), 1, 7), Vector.fromList([0, 0])), 0, x + 5000), Vector.fromList([0, 0]))
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    a + b

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "7018\n7010",
    );
}

// ── One scratch local per Vector<T>, and a re-entrant emitter ─────
//
// Holding the receiver in the per-type scratch local is what stops the
// rebuild above, and it puts the owned path on a local the emitter can
// re-enter: every operand it re-emits may itself be a `Vector.set` of
// the SAME `Vector<T>`, which stores into that same local. The three
// emitters therefore share one rule — emit everything that can
// re-enter, then store, then read — and these cells are the ones that
// notice when a read drifts back behind a re-emission. Each index below
// is written as `Vector.len(<a nested set of the same type>) - 2`, so
// the nested set is emitted at every point the outer one re-emits its
// index.

/// Owned receiver, nested owned set in the index. Cell 0 carries the
/// write and cell 1 the untouched 9; reading the nested set's array
/// instead answers 2 for cell 1.
#[test]
fn owned_receiver_survives_a_nested_set_in_its_index() {
    assert_cell(
        "own-c24",
        r#"
fn probe(x: Int) -> Int
    ? "Owned fresh receiver whose index rebuilds a nested same-type set."
    poked = Option.withDefault(Vector.set(Vector.fromList([x, 9]), Vector.len(Option.withDefault(Vector.set(Vector.fromList([1, 2]), 0, 7), Vector.fromList([]))) - 2, x + 5000), Vector.fromList([]))
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    a + b

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "7020\n7012",
    );
}

/// The clone-on-write half of the same question: a shared receiver, so
/// the set copies, and the nested set in the index lands in the local
/// between the copy and the `Some` that has to wrap it.
#[test]
fn cloned_receiver_survives_a_nested_set_in_its_index() {
    assert_cell(
        "own-c25",
        r#"
fn probe(x: Int) -> Int
    ? "Clone-on-write set whose index rebuilds a nested same-type set."
    stash = {"k" => Vector.fromList([x, 9])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = Option.withDefault(Vector.set(held, Vector.len(Option.withDefault(Vector.set(Vector.fromList([1, 2]), 0, 7), Vector.fromList([]))) - 2, x + 5000), Vector.fromList([]))
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    a + b

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "7020\n7012",
    );
}

/// And the FUSED self-keep spelling, which reaches the third emitter:
/// its clone-on-write branch read the local back twice after the index
/// had been re-emitted, once for the write target and once for the
/// result.
#[test]
fn fused_self_keep_set_survives_a_nested_set_in_its_index() {
    assert_cell(
        "own-c26",
        r#"
fn probe(x: Int) -> Int
    ? "Fused self-keep set on a shared receiver whose index rebuilds a nested same-type set."
    stash = {"k" => Vector.fromList([x, 9])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = Option.withDefault(Vector.set(held, Vector.len(Option.withDefault(Vector.set(Vector.fromList([1, 2]), 0, 7), Vector.fromList([]))) - 2, x + 5000), held)
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    a + b

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "7020\n7012",
    );
}

/// The fused self-keep set with the index OUT of bounds, which is the
/// branch the restructure changed: it now hands back the receiver
/// itself rather than an identical copy of it, and allocates nothing.
/// The answer must not move — `poked` is `held`, unchanged.
#[test]
fn fused_self_keep_set_out_of_bounds_keeps_the_receiver() {
    assert_cell(
        "own-c27",
        r#"
fn probe(x: Int) -> Int
    ? "Fused self-keep set past the end keeps the receiver unchanged."
    stash = {"k" => Vector.fromList([x, 9])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    poked = Option.withDefault(Vector.set(held, 7, 5000), held)
    a = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    b = Option.withDefault(Vector.get(poked, 1), 0 - 1)
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    a + b + Option.withDefault(Vector.get(back, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
    Console.print(String.fromInt(probe(2003)))
"#,
        "4031\n4015",
    );
}

// ── Emitted-code pin: the conservative default did not swallow the
//    fresh-local fast path ─────────────────────────────────────────

/// Compile one program to wasm-gc bytes through the same pipeline shape
/// as `aver compile --target wasm-gc` and return its printed WAT.
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

/// The two programs are identical except for `held`'s provenance —
/// fresh `Vector.fromList` vs a read out of the map — so their type /
/// helper sets match and the WAT `array.new_default` count isolates the
/// ownership decision at the `Vector.set` site. The container-read
/// variant must carry exactly one more (its clone-before-mutate); if
/// the fresh variant ever gains one, the fast path was silently
/// pessimized, and if the read variant loses its extra one, the copy
/// guard regressed.
#[test]
fn fresh_local_set_stays_in_place_while_container_read_copies() {
    let fresh = r#"
fn probe(x: Int) -> Int
    ? "Fresh receiver: the set may mutate in place."
    stash = {"k" => Vector.fromList([x, 9])}
    held = Vector.fromList([x, 9])
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
"#;
    let read = r#"
fn probe(x: Int) -> Int
    ? "Container-read receiver: the set must copy first."
    stash = {"k" => Vector.fromList([x, 9])}
    held = Option.withDefault(Map.get(stash, "k"), Vector.fromList([0, 0]))
    poked = Option.withDefault(Vector.set(held, 0, 5000), Vector.fromList([0, 0]))
    back = Option.withDefault(Map.get(stash, "k"), Vector.fromList([]))
    stored = Option.withDefault(Vector.get(back, 0), 0 - 1)
    fresh = Option.withDefault(Vector.get(poked, 0), 0 - 1)
    stored + fresh

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(probe(2011)))
"#;
    let fresh_clones = wat_of(fresh).matches("array.new_default").count();
    let read_clones = wat_of(read).matches("array.new_default").count();
    assert_eq!(
        read_clones,
        fresh_clones + 1,
        "expected the container-read variant to carry exactly one more \
         array.new_default (the clone-before-mutate) than the fresh \
         variant ({read_clones} vs {fresh_clones}) — either the fresh \
         fast path was pessimized or the copy guard regressed"
    );
}

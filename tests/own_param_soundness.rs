//! Soundness regression net for the interprocedural Vector/Map param
//! ownership refinement (`ir::mir::optimize::own_param`).
//!
//! The refinement un-flags a Vector/Map param as non-aliased when every
//! call site passes it a uniquely-owned argument, which lets a backend
//! mutate it **in place**. A false negative (un-flagging a param that is
//! actually aliased) silently corrupts data that another binding still
//! observes. These tests pin the corruption boundary:
//!
//! - each program is run through the real VM (`aver run`) — NOT `aver
//!   verify`, whose narrower `last_use` derivation masks the bug (the
//!   audit-crash lesson: a test that passes with AND without the fix
//!   proves nothing, and verify ≠ run);
//! - each asserts the value Aver's immutable model mandates; if the
//!   refinement wrongly un-flagged a shared param the in-place mutation
//!   would surface a different value and the assert fails;
//! - the headline win (`fillVector`, a linearly-threaded param the
//!   refinement *should* un-flag) is checked to still compute the right
//!   sum, so the optimization can't pass by being a no-op.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Run an Aver program via the built `aver` binary, returning trimmed
/// stdout. `extra_env` toggles the refinement off for the A/B control.
fn run_aver(name: &str, source: &str, no_own_param: bool) -> String {
    let dir = std::env::temp_dir().join(format!("aver_own_param_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    let file = dir.join(format!("{name}.av"));
    fs::write(&file, source).expect("write source");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
    cmd.arg("run").arg(&file);
    if no_own_param {
        cmd.env("AVER_NO_OWN_PARAM", "1");
    }
    let out = cmd.output().expect("spawn aver run");
    assert!(
        out.status.success(),
        "`aver run {}` failed: {}",
        file.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = fs::remove_dir_all(&dir);
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Assert a program yields the same (correct) value with the refinement
/// ON and OFF — the refinement must never change observable behaviour.
fn assert_sound(name: &str, source: &str, expected: &str) {
    let on = run_aver(name, source, false);
    let off = run_aver(name, source, true);
    assert_eq!(
        off, expected,
        "{name}: baseline (refinement OFF) must produce the immutable-model value"
    );
    assert_eq!(
        on, expected,
        "{name}: refinement ON corrupted the result — an aliased param was un-flagged and mutated in place"
    );
}

/// The adversary's counterexample: a live vector aliased through a
/// user-fn return (`identity`), then mutated through a self-keep set on
/// a param the refinement must keep flagged. Correct = `0` (the original
/// is never modified); corruption surfaces as `99`.
#[test]
fn alias_via_user_fn_return_is_not_mutated_in_place() {
    let src = r#"module Corrupt
    intent = "alias a live vector through a user-fn return, then mutate via self-keep"
    depends []
    effects [Console.print]

fn aliasIt(v: Vector<Int>) -> Vector<Int>
    ? "returns its arg — result shares backing; RULE-2 cannot see this alias source"
    v

fn clobber(w: Vector<Int>) -> Vector<Int>
    ? "self-threading set+withDefault on param w"
    Option.withDefault(Vector.set(w, 0, 99), w)

fn run() -> Int
    ? "a stays live (read at end); b aliases a; clobber(b) must NOT mutate a in place"
    a = Vector.new(2, 0)
    b = aliasIt(a)
    c = clobber(b)
    Option.withDefault(Vector.get(a, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("alias_via_return", src, "0");
}

/// The same vector handed to two params of one fn: an in-place mutation
/// of the first must not be observed through the second. Correct = `7`
/// (the read sees the original); corruption surfaces as `99`. Exercises
/// the same-slot-to-two-params rejection.
#[test]
fn same_vector_to_two_params_is_not_mutated_in_place() {
    let src = r#"module TwoParams
    intent = "same vector to two params; mutate one, read the other"
    depends []
    effects [Console.print]

fn clob2(a: Vector<Int>, b: Vector<Int>) -> Int
    ? "set on a, then read b — if a aliases b, b sees the write"
    c = Option.withDefault(Vector.set(a, 0, 99), a)
    Option.withDefault(Vector.get(b, 0), 0 - 1)

fn run() -> Int
    v = Vector.new(2, 7)
    clob2(v, v)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("two_params", src, "7");
}

/// The plain-builtin path: a vector aliased through a user-fn return,
/// bound to a local, then mutated via a single-use `Vector.set` (not the
/// self-keep `withDefault` shape). The builtin owned-mask must not take
/// `b` in place, because `b` aliases the still-live `a`. Correct = `0`;
/// corruption surfaces as `99` or `-1` (the owned take empties `a`'s
/// arena slot). Guards the `alias.rs` RULE-2 backstop that flags
/// user-fn-call results as alias sources.
#[test]
fn plain_set_on_user_fn_return_local_is_not_mutated_in_place() {
    let src = r#"module PlainCorrupt
    intent = "plain Vector.set on a user-fn-return-bound local, original live"
    depends []
    effects [Console.print]

fn aliasVec(v: Vector<Int>) -> Vector<Int>
    ? "returns its arg — result aliases backing"
    v

fn run() -> Int
    a = Vector.new(2, 0)
    b = aliasVec(a)
    c = match Vector.set(b, 0, 99)
        Option.Some(x) -> x
        Option.None -> Vector.new(2, 0 - 1)
    Option.withDefault(Vector.get(a, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("plain_set", src, "0");
}

/// The Map analogue: a map aliased through a user-fn return, then a
/// `Map.set` on the alias. The original must keep its value. Correct =
/// `7`; corruption surfaces as `99`. Exercises the same backstop +
/// builtin owned-mask on the `Map.set` path that map_build accelerates.
#[test]
fn map_set_on_user_fn_return_local_is_not_mutated_in_place() {
    let src = r#"module MapCorrupt
    intent = "Map.set on a user-fn-return-aliased map, original live"
    depends []
    effects [Console.print]

fn aliasMap(m: Map<String, Int>) -> Map<String, Int>
    ? "returns its arg — result aliases backing"
    m

fn run() -> Int
    a = Map.set({}, "k", 7)
    b = aliasMap(a)
    c = Map.set(b, "k", 99)
    Option.withDefault(Map.get(a, "k"), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("map_set", src, "7");
}

/// The headline win: a linearly-threaded vector param the refinement
/// *should* un-flag. Asserts the fill+sum result is correct (so the
/// optimization is exercised, not silently a no-op) and identical with
/// the refinement on/off. `fillVector` writes `i*i` at position `i`;
/// summing positions 0..5 of a length-5 fill gives 0+1+4+9+16 = 30.
#[test]
fn linearly_threaded_fill_sum_is_correct() {
    let src = r#"module Fill
    intent = "linearly-threaded vector fill+sum — the refinement target"
    depends []
    effects [Console.print]

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn sumVector(v: Vector<Int>, n: Int, i: Int, acc: Int) -> Int
    ? "tail-recursive sum across positions 0..n"
    match i == n
        true -> acc
        false -> sumVector(v, n, i + 1, acc + Option.withDefault(Vector.get(v, i), 0))

fn run() -> Int
    v = fillVector(Vector.new(5, 0), 5, 0)
    sumVector(v, 5, 0, 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("fill_sum", src, "30");
}

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

/// Aggregate-capture aliasing: a vector stored into a record field, then
/// own-mutated through a param the refinement would otherwise un-flag.
/// `last_use` marks the slot dead (only the record field is read after),
/// but the field aliases the same backing — so an in-place mutation would
/// corrupt it. Correct = `7`; corruption surfaces as `99`. This is the
/// class the first soundness suite missed (a shipped VM corruption found
/// by an adversarial pass): the captured slot must stay flagged.
#[test]
fn vector_aliased_into_record_field_is_not_mutated_in_place() {
    let src = r#"module Rec
    intent = "vector aliased into a record field, then own-mutated through a param"
    depends []
    effects [Console.print]

record Box
    v: Vector<Int>

fn clobber(w: Vector<Int>) -> Int
    ? "self-keep set on param w"
    c = Option.withDefault(Vector.set(w, 0, 99), w)
    Option.withDefault(Vector.get(c, 0), 0 - 1)

fn run() -> Int
    a = Vector.new(2, 7)
    box = Box(v = a)
    x = clobber(a)
    Option.withDefault(Vector.get(box.v, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("record_capture", src, "7");
}

/// Same-fn capture-then-mutate: a Vector PARAM captured into a record
/// field AND own-mutated, BOTH inside the SAME fn, on the SAME param. The
/// capture guard flags the slot, but the fixpoint (which seeds every
/// RULE-1 Vector/Map param optimistic-`true` and only descends on a
/// not-owned call-site arg) would re-`true` it — every caller passes a
/// fresh vector, so nothing pulls it back down — and the apply step would
/// then CLEAR the captured-slot bit, re-opening the corruption. This is
/// the gap the earlier `vector_aliased_into_record_field` test missed: it
/// split the capture and the mutation across two fns, so the mutated
/// param wasn't itself the captured slot. Correct = `1006`
/// (snapshot[0]=7, mutated[0]=999); corruption surfaces as `1998`
/// (the in-place set overwrote the record's snapshot too). The fix seeds
/// captured PARAM slots `false` in the ownership lattice so the proof can
/// never un-flag them.
#[test]
fn param_captured_and_mutated_in_same_fn_is_not_mutated_in_place() {
    let src = r#"module SameFnCapture
    intent = "a Vector param captured into a record AND mutated in the same fn"
    depends []
    effects [Console.print]

record Holder
    snapshot: Vector<Int>

fn captureAndMutate(v: Vector<Int>) -> Int
    ? "store v in a record, then set position 0 to 999 on v; the snapshot must read the original"
    h = Holder(snapshot = v)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    snap0 = Option.withDefault(Vector.get(h.snapshot, 0), 0 - 1)
    mut0 = Option.withDefault(Vector.get(mutated, 0), 0 - 1)
    snap0 + mut0

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    captureAndMutate(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("same_fn_capture", src, "1006");
}

/// The Map analogue of the same-fn capture-then-mutate: a Map param
/// captured into a record field AND `Map.set`-mutated in the same fn. The
/// captured slot must stay flagged so the insert does not corrupt the
/// record's snapshot. Correct = `107` (snapshot["k"]=7, mutated["k"]=100);
/// corruption surfaces as `200`.
#[test]
fn map_param_captured_and_mutated_in_same_fn_is_not_mutated_in_place() {
    let src = r#"module SameFnMapCapture
    intent = "a Map param captured into a record AND mutated in the same fn"
    depends []
    effects [Console.print]

record MapHolder
    snapshot: Map<String, Int>

fn captureAndSet(m: Map<String, Int>) -> Int
    ? "store m in a record, then Map.set k=100 on m; the snapshot must read the original 7"
    h = MapHolder(snapshot = m)
    updated = Map.set(m, "k", 100)
    snap0 = Option.withDefault(Map.get(h.snapshot, "k"), 0 - 1)
    new0 = Option.withDefault(Map.get(updated, "k"), 0 - 1)
    snap0 + new0

fn run() -> Int
    base = Map.set({}, "k", 7)
    captureAndSet(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("same_fn_map_capture", src, "107");
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

// ---------------------------------------------------------------------
// Escape-soundness regression net (the audit's classes 1–3). Each of
// these CORRUPTS on parent `main` (own_param un-flags an aliased param,
// the backend mutates it in place) and is FIXED on this branch by the
// alias-provenance + interprocedural-capture analysis. Each is a
// revert-discipline candidate: it fails on parent main, passes here.
// ---------------------------------------------------------------------

/// Class 1 — same-fn aliased capture via a `let`-rename. `w = v` aliases
/// the param `v`; `w` is captured into a record. The old capture
/// detection recorded `w`'s slot, not `v`'s, so it un-flagged `v` and the
/// in-place `Vector.set(v, …)` corrupted the record's retained handle.
/// Correct = `7` (the captured copy keeps the original); corruption = `999`.
#[test]
fn class1_let_rename_capture_vector_not_mutated_in_place() {
    let src = r#"module C1Let
    intent = "let-rename aliased capture of a Vector param"
    depends []
    effects [Console.print]

record Box
    inner: Vector<Int>

fn cap(v: Vector<Int>) -> Int
    ? "w = v aliases v; capture w; then own-mutate v; read the capture"
    w = v
    b = Box(inner = w)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(b.inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class1_let_rename_vec", src, "7");
}

/// Class 1 — same-fn aliased capture via a `match`-rename. `w = match _ ->
/// v` aliases `v` through a match arm; same corruption as the `let`-rename
/// case. Correct = `7`; corruption = `999`.
#[test]
fn class1_match_rename_capture_vector_not_mutated_in_place() {
    let src = r#"module C1Match
    intent = "match-rename aliased capture of a Vector param"
    depends []
    effects [Console.print]

record Box
    inner: Vector<Int>

fn cap(v: Vector<Int>) -> Int
    ? "w bound through a match arm aliases v; capture w; own-mutate v"
    w = match 0
        _ -> v
    b = Box(inner = w)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(b.inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class1_match_rename_vec", src, "7");
}

/// Class 1 — same-fn aliased capture through a passthrough fn. `idv(v)`
/// returns its arg, so the value captured into the record aliases `v`
/// (the RULE-2 passthrough gap). Correct = `7`; corruption = `999`.
#[test]
fn class1_passthrough_capture_vector_not_mutated_in_place() {
    let src = r#"module C1Pass
    intent = "passthrough-fn aliased capture of a Vector param"
    depends []
    effects [Console.print]

record Box
    inner: Vector<Int>

fn idv(v: Vector<Int>) -> Vector<Int>
    ? "returns its arg — result aliases backing"
    v

fn cap(v: Vector<Int>) -> Int
    ? "capture idv(v) (aliases v); then own-mutate v; read the capture"
    b = Box(inner = idv(v))
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(b.inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class1_passthrough_vec", src, "7");
}

/// Class 1 — Map analogue of the `let`-rename aliased capture. Correct =
/// `7`; corruption = `999`.
#[test]
fn class1_let_rename_capture_map_not_mutated_in_place() {
    let src = r#"module C1MapLet
    intent = "let-rename aliased capture of a Map param"
    depends []
    effects [Console.print]

record MapBox
    inner: Map<String, Int>

fn cap(m: Map<String, Int>) -> Int
    ? "w = m aliases m; capture w; then own-mutate m; read the capture"
    w = m
    b = MapBox(inner = w)
    mutated = Map.set(m, "k", 999)
    Option.withDefault(Map.get(b.inner, "k"), 0 - 1)

fn run() -> Int
    base = Map.set({}, "k", 7)
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class1_let_rename_map", src, "7");
}

/// Class 2 — live-alias skip-clone. `snap = v` aliases the param; `v` is
/// then own-mutated and `snap` is read afterwards. The param was un-flagged
/// though a live alias still observes the original backing. Correct = `7`;
/// corruption = `999`.
#[test]
fn class2_live_alias_vector_not_mutated_in_place() {
    let src = r#"module C2Vec
    intent = "live-alias of a Vector param read after an in-place mutation"
    depends []
    effects [Console.print]

fn cap(v: Vector<Int>) -> Int
    ? "snap aliases v; own-mutate v; read snap — must see the original"
    snap = v
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(snap, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class2_live_alias_vec", src, "7");
}

/// Class 2 — Map analogue of the live-alias skip-clone. Correct = `7`;
/// corruption = `999`.
#[test]
fn class2_live_alias_map_not_mutated_in_place() {
    let src = r#"module C2Map
    intent = "live-alias of a Map param read after an in-place mutation"
    depends []
    effects [Console.print]

fn cap(m: Map<String, Int>) -> Int
    ? "snap aliases m; own-mutate m; read snap — must see the original"
    snap = m
    mutated = Map.set(m, "k", 999)
    Option.withDefault(Map.get(snap, "k"), 0 - 1)

fn run() -> Int
    base = Map.set({}, "k", 7)
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class2_live_alias_map", src, "7");
}

/// Class 3 — cross-fn capture-then-mutate. `store(base)` captures `base`
/// into a record; a second fn `mutateBacking(base)` own-mutates the same
/// value. The old analysis cleared `mutateBacking`'s param (its sole
/// caller looked owned) without seeing that `base` had escaped into
/// `store`'s capture. The interprocedural capture summary now marks
/// `store` as capturing param 0, so `base` escapes in `run` and stays
/// flagged. Correct = `7`; corruption = `999`.
#[test]
fn class3_cross_fn_store_then_mutate_vector_not_mutated_in_place() {
    let src = r#"module C3Vec
    intent = "cross-fn: one fn captures the value, another own-mutates it"
    depends []
    effects [Console.print]

record Box
    items: Vector<Int>

fn store(v: Vector<Int>) -> Box
    ? "captures v into a Box"
    Box(items = v)

fn mutateBacking(v: Vector<Int>) -> Vector<Int>
    ? "own-mutate the same value"
    Option.withDefault(Vector.set(v, 0, 999), v)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    b = store(base)
    m = mutateBacking(base)
    Option.withDefault(Vector.get(b.items, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class3_cross_fn_vec", src, "7");
}

/// Class 3 — Map analogue of the cross-fn capture-then-mutate. Correct =
/// `7`; corruption = `999`.
#[test]
fn class3_cross_fn_store_then_mutate_map_not_mutated_in_place() {
    let src = r#"module C3Map
    intent = "cross-fn capture-then-mutate, Map"
    depends []
    effects [Console.print]

record MapBox
    items: Map<String, Int>

fn store(m: Map<String, Int>) -> MapBox
    ? "captures m into a MapBox"
    MapBox(items = m)

fn mutateBacking(m: Map<String, Int>) -> Map<String, Int>
    ? "own-mutate the same value"
    Map.set(m, "k", 999)

fn run() -> Int
    base = Map.set({}, "k", 7)
    b = store(base)
    upd = mutateBacking(base)
    Option.withDefault(Map.get(b.items, "k"), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class3_cross_fn_map", src, "7");
}

/// Class 2 variant — the alias is created by wrapping the param in an
/// `Option.Some(v)` (a constructor capture) and selecting it back out
/// with `Option.withDefault`. The constructor capture must flag the
/// param even though the surrounding shape looks like a `withDefault`
/// select. Correct = `7`; corruption = `999`.
#[test]
fn class2_some_wrapped_alias_not_mutated_in_place() {
    let src = r#"module C2W
    intent = "alias a Vector param by wrapping it in Option.Some, then mutate"
    depends []
    effects [Console.print]

fn cap(v: Vector<Int>) -> Int
    ? "snap selects v back out of Option.Some(v); own-mutate v; read snap"
    fb = Vector.new(3, 0)
    snap = Option.withDefault(Option.Some(v), fb)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(snap, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class2_some_wrapped", src, "7");
}

/// Precision guard at the value level: a fn that CAPTURES one Vector
/// param (`a`) while threading another (`b`) linearly must keep `a`
/// uncorrupted AND still mutate `b` in place. `box.held[0]` reads the
/// original `a` (7), `b2[0]` reads the mutated `b` (999): 7*1000+999 =
/// 7999. Over-flagging `b` would still give 7999 (correctness), so the
/// graduation half is pinned structurally in `own_param_graduation`;
/// this guards the soundness half (a must not be corrupted). A run with
/// the refinement off must agree.
#[test]
fn capture_one_thread_other_value_is_correct() {
    let src = r#"module Mixed
    intent = "capture param a, thread param b linearly"
    depends []
    effects [Console.print]

record Box
    held: Vector<Int>

fn f(a: Vector<Int>, b: Vector<Int>) -> Int
    ? "capture a into a Box; mutate b in place; read both"
    box = Box(held = a)
    b2 = Option.withDefault(Vector.set(b, 0, 999), b)
    h = Option.withDefault(Vector.get(box.held, 0), 0 - 1)
    m = Option.withDefault(Vector.get(b2, 0), 0 - 1)
    h * 1000 + m

fn run() -> Int
    va = Option.withDefault(Vector.set(Vector.new(2, 0), 0, 7), Vector.new(2, 0))
    vb = Option.withDefault(Vector.set(Vector.new(2, 0), 0, 3), Vector.new(2, 0))
    f(va, vb)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("mixed_capture_thread", src, "7999");
}

/// Class 3 — TRANSITIVE cross-fn capture. `store2` doesn't capture
/// directly; it forwards its param to `store`, which does. The
/// interprocedural capture-summary fixpoint must propagate "captures
/// param 0" from `store` to `store2`, so the value passed to `store2`
/// in `run` escapes and `mutateBacking`'s param stays flagged. This
/// exercises the summary's transitive propagation that the direct
/// class-3 case does not. Correct = `7`; corruption = `999`.
#[test]
fn class3_transitive_cross_fn_capture_not_mutated_in_place() {
    let src = r#"module C3T
    intent = "transitive cross-fn capture through a forwarding fn"
    depends []
    effects [Console.print]

record Box
    items: Vector<Int>

fn store(v: Vector<Int>) -> Box
    ? "captures v into a Box"
    Box(items = v)

fn store2(v: Vector<Int>) -> Box
    ? "forwards v to store — transitively captures param 0"
    store(v)

fn mutateBacking(v: Vector<Int>) -> Vector<Int>
    ? "own-mutate the same value"
    Option.withDefault(Vector.set(v, 0, 999), v)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    b = store2(base)
    m = mutateBacking(base)
    Option.withDefault(Vector.get(b.items, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("class3_transitive", src, "7");
}

// ---------------------------------------------------------------------
// Round-3 regression net (the third adversarial audit's two new
// classes). These CORRUPT on parent `own-param-escape-soundness` (the
// blacklist enumeration missed the escape site) and are FIXED here by
// the sound-by-construction redesign (default-flag, clear-on-whitelist).
// Each is a revert-discipline candidate: it fails on parent, passes here.
// ---------------------------------------------------------------------

/// Round 3, class (a) — value-into-collection via `Vector.set`. The
/// param `p` is stored as the VALUE/ELEMENT arg (index 2) of a
/// `Vector.set` building a vector-of-vectors, then own-mutated in place.
/// The old `alias_roots` followed only `args.first()` (the target) of a
/// set, so the element `p` was not seen to escape; the old
/// `collect_escaping_slots` only handled `MirCallee::Fn` args, not
/// builtin value args. The new whitelist scan treats every builtin arg
/// at index >= 1 as retaining. Correct = `7` (the stored copy keeps the
/// original); corruption surfaces as `999`.
#[test]
fn round3_value_into_vector_set_is_not_mutated_in_place() {
    let src = r#"module ValIntoVecSet
    intent = "param P stored as the value/element arg of Vector.set, then mutated"
    depends []
    effects [Console.print]

fn cap(p: Vector<Int>) -> Int
    ? "store p as the element of an outer vector-of-vectors; own-mutate p; read the stored copy"
    outer = Vector.new(1, p)
    mutated = Option.withDefault(Vector.set(p, 0, 999), p)
    inner = Option.withDefault(Vector.get(outer, 0), Vector.new(3, 0 - 1))
    Option.withDefault(Vector.get(inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_value_into_vector_set", src, "7");
}

/// Round 3, class (a) — value-into-collection via `Map.set`. The param
/// `p` is stored as the VALUE arg (index 2) of a `Map.set`, then
/// own-mutated. Same missed escape site as the `Vector.set` case, on the
/// `Map.set` value position. Correct = `7`; corruption surfaces as `999`.
#[test]
fn round3_value_into_map_set_is_not_mutated_in_place() {
    let src = r#"module ValIntoMapSet
    intent = "param P stored as the value arg of Map.set, then mutated"
    depends []
    effects [Console.print]

fn cap(p: Vector<Int>) -> Int
    ? "store p as a map value; own-mutate p; read the stored copy back"
    holder = Map.set({}, "key", p)
    mutated = Option.withDefault(Vector.set(p, 0, 999), p)
    inner = Option.withDefault(Map.get(holder, "key"), Vector.new(3, 0 - 1))
    Option.withDefault(Vector.get(inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_value_into_map_set", src, "7");
}

/// Round 3, class (a) — value-into-collection via a MAP LITERAL. The
/// param `p` is stored as the VALUE of a `{"k" => p}` literal entry,
/// then own-mutated. Correct = `7`; corruption surfaces as `999`.
#[test]
fn round3_value_into_map_literal_is_not_mutated_in_place() {
    let src = r#"module ValIntoMapLit
    intent = "param P stored as a value in a map literal, then mutated"
    depends []
    effects [Console.print]

fn cap(p: Vector<Int>) -> Int
    ? "store p as a map-literal value; own-mutate p; read the stored copy"
    holder = {"key" => p}
    mutated = Option.withDefault(Vector.set(p, 0, 999), p)
    inner = Option.withDefault(Map.get(holder, "key"), Vector.new(3, 0 - 1))
    Option.withDefault(Vector.get(inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_value_into_map_literal", src, "7");
}

/// Round 3, class (a) — value-into-collection via the DEFAULT arg of
/// `Vector.new`. `Vector.new(2, p)` fills both cells with `p` (every
/// cell shares its backing), then `p` is own-mutated. The old
/// `uniquely_owned` treated `Vector.new` with a non-literal default as
/// not-owned but `alias_roots`/the escape scan never flagged `p`'s
/// occurrence as the default cell. Correct = `7`; corruption = `999`.
#[test]
fn round3_value_into_vector_new_default_is_not_mutated_in_place() {
    let src = r#"module ValIntoVecNew
    intent = "param P used as the default element of Vector.new, then mutated"
    depends []
    effects [Console.print]

fn cap(p: Vector<Int>) -> Int
    ? "Vector.new(2, p) fills both cells with p; own-mutate p; read a cell"
    outer = Vector.new(2, p)
    mutated = Option.withDefault(Vector.set(p, 0, 999), p)
    inner = Option.withDefault(Vector.get(outer, 1), Vector.new(3, 0 - 1))
    Option.withDefault(Vector.get(inner, 0), 0 - 1)

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_value_into_vector_new", src, "7");
}

/// Round 3, class (b) — live-alias-at-callsite. An owned let-local
/// (`base`) is passed to a graduated linear mutator (`mutate`) while a
/// live alias (`alias = base`) remains in the caller. The old
/// `uniquely_owned` proved the arg owned by its `last_use` flag but
/// never checked whether the CALLER still observes the slot through a
/// live rename alias. `mutate` graduates and mutates `base` in place,
/// corrupting `alias`. The redesign rejects a call-site arg whose slot
/// the caller still aliases (`live_aliased`). Correct = `7` (the alias
/// keeps the original); corruption surfaces as `999`.
#[test]
fn round3_live_alias_at_callsite_is_not_mutated_in_place() {
    let src = r#"module LiveAliasCallsite
    intent = "owned local passed to a graduated callee while a live alias remains in the caller"
    depends []
    effects [Console.print]

fn mutate(w: Vector<Int>) -> Vector<Int>
    ? "graduated linear mutator: own-mutate w in place and return it"
    Option.withDefault(Vector.set(w, 0, 999), w)

fn run() -> Int
    ? "base is owned; alias aliases base; pass base to mutate, then read alias"
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    alias = base
    m = mutate(base)
    Option.withDefault(Vector.get(alias, 0), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_live_alias_at_callsite", src, "7");
}

/// Round 3, class (b) — Map analogue of live-alias-at-callsite. Correct
/// = `7`; corruption surfaces as `999`.
#[test]
fn round3_live_alias_at_callsite_map_is_not_mutated_in_place() {
    let src = r#"module LiveAliasCallsiteMap
    intent = "owned map passed to a graduated callee while a live alias remains"
    depends []
    effects [Console.print]

fn mutate(w: Map<String, Int>) -> Map<String, Int>
    ? "graduated linear mutator: Map.set w in place and return it"
    Map.set(w, "k", 999)

fn run() -> Int
    base = Map.set({}, "k", 7)
    alias = base
    m = mutate(base)
    Option.withDefault(Map.get(alias, "k"), 0 - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    assert_sound("round3_live_alias_at_callsite_map", src, "7");
}

/// The call-result class through a FIRST-CLASS FN VALUE. Every other test
/// in this file binds a call result to a local before it is used again,
/// which settles ownership on the binding rules; here `f(base)` is
/// `growth`'s argument directly, so the decision rests on the call-result
/// arm of `uniquely_owned` — and the callee is a `Fn(..)` parameter read
/// out of a slot (`MirCallee::LocalSlot`), which the pass cannot see
/// through at all. `aliasIt` hands `base` straight back, so granting
/// ownership lets `growth` empty the caller's map: correct = `6 2/7`,
/// corruption surfaces as `6 0/-1`.
#[test]
fn fn_value_result_argument_is_not_mutated_in_place() {
    let src = r#"module ViaValue
    intent = "a fn value's map result flows straight into another call"
    depends []
    effects [Console.print]

fn aliasIt(m: Map<String, Int>) -> Map<String, Int>
    ? "returns its own parameter — the result shares the caller's map"
    m

fn growth(m: Map<String, Int>, n: Int) -> Int
    ? "threads the map linearly, then reports its size"
    match n == 0
        true -> Map.len(m)
        false -> growth(Map.set(m, "g{n}", n), n - 1)

fn render(m: Map<String, Int>) -> String
    ? "size plus the value under a"
    "{Map.len(m)}/{Option.withDefault(Map.get(m, "a"), 0 - 1)}"

fn viaValue(f: Fn(Map<String, Int>) -> Map<String, Int>, base: Map<String, Int>) -> String
    ? "the fn value's result is the next call's argument; base is read after"
    grown = growth(f(base), 4)
    "{grown} {render(base)}"

fn run() -> String
    base = Map.set(Map.set({}, "a", 7), "b", 8)
    viaValue(aliasIt, base)

fn main() -> Unit
    ! [Console.print]
    Console.print(run())
"#;
    assert_sound("fn_value_result_arg", src, "6 2/7");
}

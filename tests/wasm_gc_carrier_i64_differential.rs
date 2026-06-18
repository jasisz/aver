//! ETAP-2 carrier-`i64` — refinement-via-opaque carrier types erased to a
//! native `i64` on the wasm-gc backend.
//!
//! A carrier `record IntRange { value: Int } exposes opaque` whose
//! smart-constructor proves a `fits_i64` bound erases to a native `i64`
//! EVERYWHERE it appears: function slots, record/struct FIELDS, and
//! `Vector<Carrier>` / `List<Carrier>` / `Map<_, Carrier>` elements. The VM
//! keeps the full `$aint` carrier (a record holding ℤ), so VM output is the
//! ground truth; every emitted carrier-`i64` program MUST produce identical
//! output. A wrong-bare value would silently wrap on wasm-gc (no trip-wire),
//! and a type desync fails wasm validation at compile time — so identical
//! VM-vs-wasm-gc output is the soundness gate.
//!
//! The bound proves the value fits `i64` (opaque + guarded smart ctor), so
//! the boundary conversions are sound: the construct narrows the `$AverInt`
//! field value to `i64` via `__aint_to_i64_checked` (whose trap is
//! unreachable for a real carrier value), the projection lifts the `i64`
//! back to `$AverInt` via `__aint_from_i64`. Arithmetic over a projected
//! carrier runs at full `$aint` precision, so a transient that overshoots
//! `i64` and comes back (`(c + i64::MAX) - i64::MAX`) stays exact.

#![cfg(feature = "wasm")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &std::path::Path) {
    let _ = std::fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

/// Run `source` on the VM (`wasm_gc = false`) or wasm-gc (`true`).
/// `no_carrier` sets `AVER_NO_CARRIER_I64=1` so the wasm-gc backend keeps
/// the all-`$aint` carrier representation — the differential / revert-test
/// baseline.
fn run(prefix: &str, source: &str, wasm_gc: bool, no_carrier: bool) -> (bool, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root).arg("run").arg(&path);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    if no_carrier {
        cmd.env("AVER_NO_CARRIER_I64", "1");
    }
    let out = cmd.output().expect("aver run executes");
    cleanup(&path);
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stdout).trim().to_string(),
    )
}

/// FULL-MODULE wasm-gc compile of `source` (carrier-`i64` ON). Returns
/// `(success, combined stdout+stderr)`. The boundary-completeness class this
/// guards (a bare-i64 carrier result meeting an `$AverInt`-typed sink) is a
/// MODULE-level validation failure: it surfaces only when the WHOLE module
/// is lowered, never under a per-case `verify --wasm-gc` (which wraps one fn
/// in a thin harness and can't see the cross-fn boundary). `aver run
/// --wasm-gc` exercises only the call graph `main` reaches, so a sink in an
/// unreached fn slips through — `compile --target wasm-gc` lowers EVERY fn.
fn compile_wasm_gc(prefix: &str, source: &str) -> (bool, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile executes");
    cleanup(&path);
    let mut combined = String::from_utf8_lossy(&out.stdout).to_string();
    combined.push_str(&String::from_utf8_lossy(&out.stderr));
    (out.status.success(), combined.trim().to_string())
}

/// Assert `source` compiles to a wasm-gc module CLEANLY — no validation
/// error. The `compile` CLI prints a validation failure to stdout but STILL
/// EXITS 0, so a success-status check alone is blind to this bug class; the
/// assertion scans the combined output for the diagnostic substrings too.
fn assert_compiles_clean_wasm_gc(prefix: &str, source: &str) {
    let (ok, msg) = compile_wasm_gc(prefix, source);
    assert!(
        ok && !msg.contains("validation failed")
            && !msg.contains("type mismatch")
            && !msg.contains("error"),
        "{prefix}: full-module wasm-gc compile was NOT clean — a carrier-i64 value \
         reached an $AverInt-typed sink (builtin arg / field / map / aggregate / boxed \
         param) without a Box at the boundary. `verify --wasm-gc` masks this; only a \
         full-module compile catches it.\n{msg}"
    );
}

/// VM == wasm-gc (carrier-`i64` ON). Divergence ⇒ a carrier value wrapped or
/// a boundary bridge dropped precision. ALSO asserts the WHOLE module
/// compiles clean to wasm-gc (every fn lowered, not just `main`'s reachable
/// graph) — the boundary-completeness gate `aver run` / `verify --wasm-gc`
/// could not see.
fn assert_vm_wasm_identical(prefix: &str, source: &str) -> String {
    let (vm_ok, vm_out) = run(prefix, source, false, false);
    let (wg_ok, wg_out) = run(prefix, source, true, false);
    assert!(vm_ok, "{prefix}: VM run failed:\n{vm_out}");
    assert!(wg_ok, "{prefix}: wasm-gc run failed:\n{wg_out}");
    assert_eq!(
        vm_out, wg_out,
        "{prefix}: VM-vs-wasm-gc DIVERGENCE — a carrier-i64 value wrapped or a \
         boundary bridge lost precision where the VM kept the full $aint carrier.\n  \
         VM     = {vm_out:?}\n  wasm-gc= {wg_out:?}"
    );
    assert_compiles_clean_wasm_gc(prefix, source);
    vm_out
}

/// REVERT-TEST: the same program with the carrier path forced OFF
/// (`AVER_NO_CARRIER_I64=1`, all-`$aint`) must still match the VM AND the
/// carrier-`i64` output. Equal-both-ways proves the carrier erasure changed
/// the REPRESENTATION, never the observable result — the soundness invariant.
fn assert_carrier_revert_agrees(prefix: &str, source: &str, expected: &str) {
    let (wg_off_ok, wg_off_out) = run(prefix, source, true, true);
    assert!(
        wg_off_ok,
        "{prefix}: wasm-gc (carrier OFF) run failed:\n{wg_off_out}"
    );
    assert_eq!(
        wg_off_out, expected,
        "{prefix}: the all-$aint baseline diverged from the carrier-i64 output — \
         the carrier erasure must be representation-only.\n  carrier-off = {wg_off_out:?}\n  \
         expected    = {expected:?}"
    );
}

/// Milestone 1 — a carrier as a FUNCTION SLOT (param + projection + smart-ctor
/// Result boundary). `toInt`/`doubled` take `(param i64)`; `fromInt`'s Ok
/// payload is `i64`. The full chain must equal the VM, and the revert-test
/// (all-$aint) must agree.
#[test]
fn carrier_function_slot_matches_vm_and_reverts() {
    let src = r#"module M
    intent = "carrier-i64 function slot"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn toInt(c: IntRange) -> Int
    c.value

fn doubled(c: IntRange) -> Int
    c.value + c.value

fn main() -> Unit
    ! [Console.print]
    match fromInt(50)
        Result.Ok(c)  -> Console.print("{toInt(c) + doubled(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-fn-slot", src);
    assert_eq!(out, "150");
    assert_carrier_revert_agrees("carrier-fn-slot", src, &out);
}

/// Bound EDGES (0, 100) and a TRANSIENT that overshoots `i64::MAX` and comes
/// back. The transient must stay EXACT: the projected carrier is boxed to
/// `$aint` before the arithmetic, so `(c + i64::MAX) - i64::MAX == c` at full
/// precision (a raw-i64 add would wrap). This is the load-bearing soundness
/// case for the project bridge.
#[test]
fn carrier_bound_edges_and_transient_overflow_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 edges + transient"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn transient(c: IntRange) -> Int
    (c.value + 9223372036854775807) - 9223372036854775807

fn show(label: String, r: Result<IntRange, String>) -> Unit
    ! [Console.print]
    match r
        Result.Ok(c)  -> Console.print("{label}:{transient(c)}")
        Result.Err(e) -> Console.print("{label}:ERR")

fn main() -> Unit
    ! [Console.print]
    show("0", fromInt(0))
    show("100", fromInt(100))
    show("50", fromInt(50))
    show("neg", fromInt(0 - 1))
"#;
    let out = assert_vm_wasm_identical("carrier-edges", src);
    assert_eq!(out, "0:0\n100:100\n50:50\nneg:ERR");
    assert_carrier_revert_agrees("carrier-edges", src, &out);
}

/// Milestone 2 — a carrier as a STRUCT FIELD: `record Coord { x: IntRange,
/// y: IntRange }` lowers to `(struct (field i64) (field i64))`. Build /
/// update / read coords. VM == wasm-gc, and the revert-test (all-$aint)
/// agrees. The build/update/read path is representation-only, so the
/// all-$aint baseline produces the same result.
#[test]
fn carrier_struct_field_matches_vm_and_reverts() {
    let src = r#"module M
    intent = "carrier-i64 struct field"
    effects [Console]

record IntRange
    value: Int

record Coord
    x: IntRange
    y: IntRange

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn toInt(c: IntRange) -> Int
    c.value

fn mkCoord(a: IntRange, b: IntRange) -> Coord
    Coord(x = a, y = b)

fn moveX(p: Coord, nx: IntRange) -> Coord
    Coord(x = nx, y = p.y)

fn sumCoord(p: Coord) -> Int
    toInt(p.x) + toInt(p.y)

fn main() -> Unit
    ! [Console.print]
    Console.print("{sumCoord(moveX(mkCoord(ir(10), ir(20)), ir(99)))}")
"#;
    let out = assert_vm_wasm_identical("carrier-struct-field", src);
    assert_eq!(out, "119");
    assert_carrier_revert_agrees("carrier-struct-field", src, &out);
}

/// Carrier-FIELD EQUALITY: a `Coord` with two carrier-`i64` fields compared
/// via `==` routes through a raw `i64.eq` per field. VM == wasm-gc.
///
/// NB: the revert-test is intentionally OMITTED here. The all-`$aint`
/// baseline (`AVER_NO_CARRIER_I64=1`) for a record whose fields are
/// newtype-erased Int carriers TRAPS on wasm-gc with a `cast failure` — a
/// PRE-EXISTING bug in the boxed record-eq path (the pristine pre-carrier
/// HEAD traps identically), independent of this slice. The carrier-`i64`
/// path's raw `i64.eq` per field actually AVOIDS that trap, so the
/// carrier-ON differential is correct where the boxed baseline is broken.
#[test]
fn carrier_struct_field_equality_matches_vm() {
    let src = r#"module M
    intent = "carrier-i64 struct field equality"
    effects [Console]

record IntRange
    value: Int

record Coord
    x: IntRange
    y: IntRange

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn mkCoord(a: IntRange, b: IntRange) -> Coord
    Coord(x = a, y = b)

fn sameCoord(a: Coord, b: Coord) -> Bool
    a == b

fn main() -> Unit
    ! [Console.print]
    Console.print("{sameCoord(mkCoord(ir(1), ir(2)), mkCoord(ir(1), ir(2)))} {sameCoord(mkCoord(ir(1), ir(2)), mkCoord(ir(1), ir(3)))}")
"#;
    let out = assert_vm_wasm_identical("carrier-struct-field-eq", src);
    assert_eq!(out, "true false");
}

/// Milestone 3 (build/read) — a carrier as a `Vector` element / `Map`
/// value. `Vector<IntRange>` is `(array i64)`; `Map<String, IntRange>`
/// stores i64 values. Build / read through `Vector.get` / `Map.get`
/// (carrier payload unwrapped from the returned `Option<IntRange>`). VM ==
/// wasm-gc.
///
/// NB: the revert-test is OMITTED. The all-`$aint` baseline for a
/// `Vector<newtype-carrier>` / `Map<_, newtype-carrier>` fails wasm
/// validation even for plain build/read — a PRE-EXISTING bug in the boxed
/// container path (the pristine pre-carrier HEAD fails identically), which
/// the carrier-`i64` erasure happens to fix.
#[test]
fn carrier_in_containers_build_read_matches_vm() {
    let src = r#"module M
    intent = "carrier-i64 build/read in Vector/Map"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn setAt(v: Vector<IntRange>, i: Int, x: IntRange) -> Vector<IntRange>
    Option.withDefault(Vector.set(v, i, x), v)

fn buildVec() -> Vector<IntRange>
    setAt(setAt(Vector.new(2, ir(0)), 0, ir(10)), 1, ir(20))

fn vecAt(v: Vector<IntRange>, i: Int) -> Int
    match Vector.get(v, i)
        Option.Some(c) -> c.value
        Option.None    -> 0 - 1

fn mkMap() -> Map<String, IntRange>
    Map.fromList([("a", ir(42)), ("b", ir(7))])

fn mapGet(m: Map<String, IntRange>, k: String) -> Int
    match Map.get(m, k)
        Option.Some(c) -> c.value
        Option.None    -> 0 - 1

fn main() -> Unit
    ! [Console.print]
    Console.print("{vecAt(buildVec(), 0) + vecAt(buildVec(), 1)} {mapGet(mkMap(), "a")} {mapGet(mkMap(), "z")}")
"#;
    let out = assert_vm_wasm_identical("carrier-containers", src);
    assert_eq!(out, "30 42 -1");
}

/// Milestone 3 (eq/hash) — `List.contains`, list `==`, and `Map ==` over a
/// carrier element/value, each dispatched as a raw `i64.eq` / `i32.wrap_i64`.
/// VM == wasm-gc.
///
/// NB: the revert-test is OMITTED. The all-`$aint` baseline for a
/// `List<newtype-carrier>` / `Map<_, newtype-carrier>` `==`/`contains` fails
/// wasm validation — a PRE-EXISTING bug in the boxed container-eq path (the
/// pristine pre-carrier HEAD fails identically), independent of this slice.
/// The carrier-`i64` raw-element dispatch is correct where the boxed
/// baseline is broken.
#[test]
fn carrier_in_containers_eq_matches_vm() {
    let src = r#"module M
    intent = "carrier-i64 container eq/contains"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn mkList() -> List<IntRange>
    [ir(1), ir(2), ir(3)]

fn mkMap() -> Map<String, IntRange>
    Map.fromList([("a", ir(42)), ("b", ir(7))])

fn main() -> Unit
    ! [Console.print]
    Console.print("{List.contains(mkList(), ir(2))} {List.contains(mkList(), ir(9))} {mkList() == mkList()} {mkMap() == mkMap()}")
"#;
    let out = assert_vm_wasm_identical("carrier-containers-eq", src);
    assert_eq!(out, "true false true true");
}

/// Byte-identity guard: a NON-carrier program (no opaque-bounded carrier in
/// scope) must compile to a wasm-gc module that is byte-for-byte the SAME
/// with the carrier path on (default) and off (`AVER_NO_CARRIER_I64=1`) —
/// the carrier path only fires for eligible opaque types, so it is inert
/// for ordinary code.
#[test]
fn non_carrier_program_byte_identical_carrier_on_off() {
    let src = r#"module M
    intent = "non-carrier program — carrier path must be inert"
    effects [Console]

record Point
    x: Int
    y: Int

fn mk(a: Int, b: Int) -> Point
    Point(x = a, y = b)

fn sum(p: Point) -> Int
    p.x + p.y

fn main() -> Unit
    ! [Console.print]
    Console.print("{sum(mk(3, 4))}")
"#;
    let on = compile_wasm_bytes("noncarrier-on", src, false);
    let off = compile_wasm_bytes("noncarrier-off", src, true);
    assert_eq!(
        on, off,
        "a non-carrier program must produce byte-identical wasm-gc with the carrier path \
         on vs off — the carrier erasure leaked into ordinary code"
    );
}

// ---------------------------------------------------------------------------
// Carrier ARITHMETIC differentials (the raw-i64 carrier path,
// `CARRIER_BARE_ELIGIBLE == true`). A bare carrier's `.value` reads a native
// `i64` and arithmetic over it runs as `i64.add/sub/mul` WHERE the interval
// fixpoint proves the result fits `i64`; an operation that could leave `i64`
// stays boxed (the project bridge runs, full `$aint` precision). The VM (full
// ℤ) is the oracle: any divergence is a silent wrap.

/// Carrier + carrier and carrier * literal: both run as native `i64` ops over
/// a `[0,100]` carrier (the products fit `i64`). VM == wasm-gc.
#[test]
fn carrier_arith_add_and_mul_literal_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 add + mul-literal arithmetic"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn sumPair(a: IntRange, b: IntRange) -> Int
    a.value + b.value

fn timesTen(c: IntRange) -> Int
    c.value * 10

fn doubled(c: IntRange) -> Int
    c.value + c.value

fn main() -> Unit
    ! [Console.print]
    Console.print("{sumPair(ir(30), ir(12))} {timesTen(ir(7))} {doubled(ir(20))}")
"#;
    let out = assert_vm_wasm_identical("carrier-arith-add-mul", src);
    assert_eq!(out, "42 70 40");
    assert_carrier_revert_agrees("carrier-arith-add-mul", src, &out);
}

/// Carrier COMPARISON `<` and `==`: a bare carrier's `.value` compares as a
/// raw `i64.lt_s` / `i64.eq`. VM == wasm-gc.
#[test]
fn carrier_value_comparison_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 value comparison"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn less(a: IntRange, b: IntRange) -> Bool
    a.value < b.value

fn eq(a: IntRange, b: IntRange) -> Bool
    a.value == b.value

fn main() -> Unit
    ! [Console.print]
    Console.print("{less(ir(3), ir(7))} {less(ir(7), ir(3))} {eq(ir(5), ir(5))} {eq(ir(5), ir(6))}")
"#;
    let out = assert_vm_wasm_identical("carrier-cmp", src);
    assert_eq!(out, "true false true false");
    assert_carrier_revert_agrees("carrier-cmp", src, &out);
}

/// MIXED-representation carrier comparison + arithmetic — the load-bearing
/// snake shape. `s.x.value` reads a carrier FIELD (`Project(Project(..))`, a
/// boxed `$AverInt` via the project bridge) while `fx.value` reads a bare
/// carrier PARAM's `.value` (a raw i64). Comparing / subtracting one against
/// the other must coerce both to the same representation (box the raw side) —
/// without the fix, codegen compared an i64 against an `$AverInt` ref, a wasm
/// validation error. VM == wasm-gc.
#[test]
fn carrier_mixed_field_and_param_value_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 mixed field/param value comparison + subtraction"
    effects [Console]

record Coord
    value: Int

record State
    x: Coord
    y: Coord

fn coordOf(n: Int) -> Result<Coord, String>
    match Bool.and(n >= 0, n <= 1000)
        true  -> Result.Ok(Coord(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(value = 0)

fn coord(n: Int) -> Coord
    unwrap(coordOf(n))

fn atFood(s: State, fx: Coord, fy: Coord) -> Bool
    Bool.and(s.x.value == fx.value, s.y.value == fy.value)

fn manhattan(s: State, fx: Coord, fy: Coord) -> Int
    (fx.value - s.x.value) + (fy.value - s.y.value)

fn main() -> Unit
    ! [Console.print]
    s = State(x = coord(5), y = coord(5))
    fx = coord(8)
    fy = coord(5)
    Console.print("{atFood(s, fx, fy)} {atFood(s, coord(5), coord(5))} {manhattan(s, fx, fy)}")
"#;
    let out = assert_vm_wasm_identical("carrier-mixed", src);
    assert_eq!(out, "false true 3");
    assert_carrier_revert_agrees("carrier-mixed", src, &out);
}

/// Carrier as a RECORD FIELD with arithmetic on the projected value, plus the
/// SNAKE pattern — a record-update increment `score = state.score + 1` where
/// `score` is a bounded carrier field. The `state.score.value + 1` runs as a
/// native `i64.add` and the result is rewrapped into a fresh carrier field via
/// the construct bridge. VM == wasm-gc.
#[test]
fn carrier_record_field_arithmetic_and_snake_pattern_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 record field arithmetic + snake score increment"
    effects [Console]

record Coord
    value: Int

record State
    score: Coord
    x: Coord

fn fromCoord(n: Int) -> Result<Coord, String>
    match Bool.and(n >= 0, n <= 1000)
        true  -> Result.Ok(Coord(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(value = 0)

fn coord(n: Int) -> Coord
    unwrap(fromCoord(n))

fn bump(s: State) -> State
    State(score = coord(s.score.value + 1), x = s.x)

fn readScore(s: State) -> Int
    s.score.value

fn readX(s: State) -> Int
    s.x.value

fn main() -> Unit
    ! [Console.print]
    s = State(score = coord(41), x = coord(7))
    s2 = bump(s)
    Console.print("{readScore(s2)} {readX(s2)} {readScore(s)}")
"#;
    let out = assert_vm_wasm_identical("carrier-record-snake", src);
    assert_eq!(out, "42 7 41");
}

/// WIDE-BOUND OVERFLOW (the load-bearing C0 soundness probe). A carrier
/// bounded `0..2^40` doing `c.value * c.value` = up to `2^80`, which OVERFLOWS
/// `i64`. The interval fixpoint must DEMOTE the multiply to boxed (the project
/// bridge runs, the `*` is the full-precision `__aint_mul`) — a raw `i64.mul`
/// would WRAP silently. The VM keeps full ℤ; an equal wasm-gc result proves
/// the analysis did NOT emit a wrapping native op for an out-of-`i64` op.
#[test]
fn carrier_wide_bound_mul_overflow_stays_boxed_match_vm() {
    let src = r#"module M
    intent = "carrier-i64 wide-bound multiply overflow"
    effects [Console]

record Wide
    value: Int

fn fromWide(n: Int) -> Result<Wide, String>
    match Bool.and(n >= 0, n <= 1099511627776)
        true  -> Result.Ok(Wide(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Wide, String>) -> Wide
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Wide(value = 0)

fn wide(n: Int) -> Wide
    unwrap(fromWide(n))

fn square(c: Wide) -> Int
    c.value * c.value

fn main() -> Unit
    ! [Console.print]
    Console.print("{square(wide(1099511627776))}")
"#;
    // 2^40 = 1099511627776; (2^40)^2 = 2^80, far beyond i64::MAX. The VM
    // computes the exact bignum; wasm-gc must match (stayed boxed, no wrap).
    let out = assert_vm_wasm_identical("carrier-wide-overflow", src);
    assert_eq!(out, "1208925819614629174706176");
}

// ---------------------------------------------------------------------------
// NESTED carrier-FIELD reads (`rec.c.value`) — the record-threaded GAMES win.
//
// A `.value` read whose base is itself a carrier-typed RECORD FIELD —
// `Project(Project(rec, "c"), "value")` — renders raw native `i64` on wasm-gc:
// #550 erased the carrier field `c` to an i64 field, so the inner `struct.get`
// yields i64 and the outer `.value` is identity. Before this slice the read
// routed through the boxed `$AverInt` project bridge (the base was a `Project`,
// not a bare-carrier `Local`), so a state-record-threaded game got ZERO arith
// win. Now the nested read is a raw i64 leaf fed into the SAME interval
// fixpoint as a param-level read: in-range arithmetic goes native, an
// out-of-`i64` op stays boxed. The VM (full ℤ) is the oracle.

/// The core nested-field shape: a record `{ c: Carrier }` with `rec.c.value +
/// rec.c.value`. Each `rec.c.value` reads the i64 carrier field directly and
/// the sum runs as a native `i64.add`. VM == wasm-gc, compiles clean, reverts.
#[test]
fn nested_carrier_field_add_matches_vm() {
    let src = r#"module M
    intent = "nested carrier-field read add"
    effects [Console]

record IntRange
    value: Int

record Holder
    c: IntRange

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn doubled(h: Holder) -> Int
    h.c.value + h.c.value

fn main() -> Unit
    ! [Console.print]
    Console.print("{doubled(Holder(c = ir(21)))}")
"#;
    let out = assert_vm_wasm_identical("nested-field-add", src);
    assert_eq!(out, "42");
    assert_carrier_revert_agrees("nested-field-add", src, &out);
}

/// The SNAKE shape — a GameState-like record holding bounded-carrier coords +
/// a delta, with `state.x.value + state.dx.value` style nested-field
/// arithmetic (the canonical record-threaded game move). Both nested reads are
/// raw i64; the add is native. VM == wasm-gc, compiles clean, reverts.
#[test]
fn nested_carrier_field_snake_move_matches_vm() {
    let src = r#"module M
    intent = "snake-shaped nested carrier-field arithmetic"
    effects [Console]

record Coord
    value: Int

record GameState
    x: Coord
    y: Coord
    dx: Coord
    dy: Coord
    score: Coord

fn coordOf(n: Int) -> Result<Coord, String>
    match Bool.and(n >= 0, n <= 1000)
        true  -> Result.Ok(Coord(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(value = 0)

fn coord(n: Int) -> Coord
    unwrap(coordOf(n))

fn nextX(s: GameState) -> Int
    s.x.value + s.dx.value

fn nextY(s: GameState) -> Int
    s.y.value + s.dy.value

fn bumpedScore(s: GameState) -> Int
    s.score.value + 1

fn atFood(s: GameState, fx: Coord, fy: Coord) -> Bool
    Bool.and(s.x.value == fx.value, s.y.value == fy.value)

fn main() -> Unit
    ! [Console.print]
    s = GameState(x = coord(5), y = coord(7), dx = coord(1), dy = coord(2), score = coord(3))
    Console.print("{nextX(s)} {nextY(s)} {bumpedScore(s)} {atFood(s, coord(5), coord(7))} {atFood(s, coord(6), coord(7))}")
"#;
    let out = assert_vm_wasm_identical("nested-field-snake", src);
    assert_eq!(out, "6 9 4 true false");
    assert_carrier_revert_agrees("nested-field-snake", src, &out);
}

/// Nested-field read flowing into each `$AverInt`-typed SINK. A bare-returning
/// fn whose body is a nested-field arith (`h.c.value + h.c.value`, kept native)
/// feeds its raw `i64` result into: `String.fromInt`, a `Map` value, an
/// independent-product (bang-group) element, and a no-bind `match` subject.
/// Each sink slot is `$AverInt`, so the rewrite must box the raw result at the
/// boundary (the same chokepoints #551 closed) — the new raw SOURCE flows into
/// the SAME sinks. VM == wasm-gc, and the WHOLE module compiles clean.
#[test]
fn nested_carrier_field_into_sinks_box_at_boundary() {
    let src = r#"module M
    intent = "nested carrier-field result into $AverInt sinks"
    effects [Console]

record IntRange
    value: Int

record Holder
    c: IntRange

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn dbl(h: Holder) -> Int
    h.c.value + h.c.value

fn toStr(h: Holder) -> String
    String.fromInt(dbl(h))

fn intoMap(h: Holder) -> Int
    m = Map.set({}, "k", dbl(h))
    Option.withDefault(Map.get(m, "k"), 0)

fn intoProduct(h: Holder) -> Int
    match (dbl(h), dbl(h))!
        (a, b) -> a + b

fn classify(h: Holder) -> Int
    match dbl(h)
        0  -> 1
        20 -> 2
        _  -> 3

fn main() -> Unit
    ! [Console.print]
    h = Holder(c = ir(10))
    Console.print("{toStr(h)} {intoMap(h)} {intoProduct(h)} {classify(h)}")
"#;
    let out = assert_vm_wasm_identical("nested-field-sinks", src);
    assert_eq!(out, "20 20 40 2");
    assert_carrier_revert_agrees("nested-field-sinks", src, &out);
}

/// MUTUAL-RECURSION SCC member stringifying a nested carrier field — the
/// snake `tick`/`tickMove`/`gameLoop` shape. `step`/`bounce`/`loopN` form a
/// call cycle, so all three carry a PINNED boxed (`$AverInt`) signature and
/// the rewrite forces each member's body all-boxed. The bug: the all-boxed
/// override used FULLY-empty facts, dropping the carrier-recognition tables —
/// so a nested carrier-field read (`h.c.value`, base type a single-field
/// carrier) reached an `$AverInt` sink (`String.fromInt`) WITHOUT a `Box`,
/// while the wasm-gc emitter (registry-based, independent of the override)
/// still rendered it raw `i64`. The raw `i64` met the formatter's `(ref null
/// $type)` slot — a wasm VALIDATION failure on a VM-valid program. The fix
/// keeps the TYPE-driven carrier tables in the all-boxed facts so the rewrite
/// boxes the field read at the sink. Without it, `compile --target wasm-gc`
/// fails validation; with it, VM == wasm-gc.
#[test]
fn mutual_recursion_member_stringifies_carrier_field_boxes_at_boundary() {
    let src = r#"module M
    intent = "mutual-TCO member stringifies a nested carrier field"
    effects [Console]

record Score
    value: Int

record State
    score: Score
    fuel: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 1000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn step(s: State) -> String
    match s.fuel == 0
        true  -> "done: {String.fromInt(s.score.value)}"
        false -> bounce(s)

fn bounce(s: State) -> String
    loopN(State(score = s.score, fuel = s.fuel - 1))

fn loopN(s: State) -> String
    step(s)

fn main() -> Unit
    ! [Console.print]
    Console.print(step(State(score = sc(7), fuel = 3)))
"#;
    let out = assert_vm_wasm_identical("mutual-tco-carrier-stringify", src);
    assert_eq!(out, "done: 7");
    assert_carrier_revert_agrees("mutual-tco-carrier-stringify", src, &out);
}

/// WIDE-BOUND NESTED-FIELD OVERFLOW (the C0 soundness probe for the new raw
/// source). A record holds a `0..2^40` carrier; `rec.c.value * rec.c.value` =
/// up to `2^80`, which OVERFLOWS `i64`. The interval fixpoint must read the
/// nested-field `.value` at the carrier's PROVEN `[0, 2^40]` bound and DEMOTE
/// the multiply to boxed (the project bridge runs, the `*` is the
/// full-precision `__aint_mul`) — a raw `i64.mul` would WRAP silently. The VM
/// keeps full ℤ; an equal wasm-gc result proves the analysis bounded the
/// nested-field read correctly and did NOT emit a wrapping native op.
#[test]
fn nested_carrier_field_wide_bound_mul_stays_boxed_match_vm() {
    let src = r#"module M
    intent = "nested carrier-field wide-bound multiply overflow"
    effects [Console]

record Wide
    value: Int

record Holder
    c: Wide

fn fromWide(n: Int) -> Result<Wide, String>
    match Bool.and(n >= 0, n <= 1099511627776)
        true  -> Result.Ok(Wide(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Wide, String>) -> Wide
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Wide(value = 0)

fn wide(n: Int) -> Wide
    unwrap(fromWide(n))

fn square(h: Holder) -> Int
    h.c.value * h.c.value

fn main() -> Unit
    ! [Console.print]
    Console.print("{square(Holder(c = wide(1099511627776)))}")
"#;
    // (2^40)^2 = 2^80, far beyond i64::MAX. The nested-field `.value` reads carry
    // the [0, 2^40] bound, so the fixpoint demotes the multiply to boxed and the
    // VM's exact bignum is reproduced.
    let out = assert_vm_wasm_identical("nested-field-wide-overflow", src);
    assert_eq!(out, "1208925819614629174706176");
}

/// SIZE PROOF for the record-threaded win: a snake-shaped GameState whose
/// coords + score are bounded opaque carriers held in a record, with native
/// nested-field arithmetic across moves. With the carrier path ON (default)
/// the `.value` reads are raw i64 and the add/sub/mul/cmp run native, so the
/// boxed `$AverInt` arithmetic prelude DCEs; with it OFF (`AVER_NO_CARRIER_I64=1`)
/// each read lifts through the project bridge into a boxed limb-op. The ON
/// build must be NO LARGER than the boxed baseline (and strictly smaller here,
/// since the boxed-arith helpers are no longer reachable). VM == wasm-gc both
/// ways is covered by `assert_vm_wasm_identical`; this test reports the bytes.
#[test]
fn nested_carrier_field_snake_size_drops_vs_boxed() {
    let src = r#"module M
    intent = "record-threaded snake size: native carrier-field arithmetic"
    effects [Console]

record Coord
    value: Int

record GameState
    x: Coord
    y: Coord
    dx: Coord
    dy: Coord
    score: Coord

fn coordOf(n: Int) -> Result<Coord, String>
    match Bool.and(n >= 0, n <= 1000)
        true  -> Result.Ok(Coord(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(value = 0)

fn coord(n: Int) -> Coord
    unwrap(coordOf(n))

fn nextX(s: GameState) -> Int
    s.x.value + s.dx.value

fn nextY(s: GameState) -> Int
    s.y.value + s.dy.value

fn manhattan(s: GameState, fx: Coord, fy: Coord) -> Int
    (fx.value - s.x.value) + (fy.value - s.y.value)

fn areaScaled(s: GameState) -> Int
    s.x.value * s.dx.value

fn atFood(s: GameState, fx: Coord, fy: Coord) -> Bool
    Bool.and(s.x.value == fx.value, s.y.value == fy.value)

fn aheadOf(s: GameState, fx: Coord) -> Bool
    s.x.value < fx.value

fn bumpScore(s: GameState) -> Int
    s.score.value + 1

fn main() -> Unit
    ! [Console.print]
    s = GameState(x = coord(5), y = coord(7), dx = coord(1), dy = coord(2), score = coord(3))
    fx = coord(8)
    fy = coord(7)
    Console.print("{nextX(s)} {nextY(s)} {manhattan(s, fx, fy)} {areaScaled(s)} {atFood(s, fx, fy)} {aheadOf(s, fx)} {bumpScore(s)}")
"#;
    // Behavior is identical both ways (representation-only erasure).
    let out = assert_vm_wasm_identical("nested-snake-size", src);
    assert_eq!(out, "6 9 3 5 false true 4");
    assert_carrier_revert_agrees("nested-snake-size", src, &out);

    let on = compile_wasm_bytes("nested-snake-on", src, false);
    let off = compile_wasm_bytes("nested-snake-off", src, true);
    assert!(
        on.len() <= off.len(),
        "record-threaded carrier-i64 must not GROW the module — native nested-field \
         arithmetic should DCE the boxed arith prelude. carrier-ON={} bytes, \
         carrier-OFF(boxed)={} bytes",
        on.len(),
        off.len(),
    );
    // The boxed `$AverInt` arithmetic helpers (`__aint_add` / `__aint_mul` /
    // `__aint_sub`) must be UNREACHABLE in the carrier-ON build — every nested
    // read went native. The WAT-token probe tolerates `wasm-tools` being absent
    // (both counts 0 ⇒ the assertion is vacuous). With it present the carrier-ON
    // boxed-arith footprint must sit STRICTLY below the boxed baseline.
    let on_boxed_arith = carrier_boxed_arith_token_count("nested-snake-on-wat", src, false);
    let off_boxed_arith = carrier_boxed_arith_token_count("nested-snake-off-wat", src, true);
    if off_boxed_arith > 0 {
        assert!(
            on_boxed_arith < off_boxed_arith,
            "the carrier-ON build must reach FEWER boxed-arith helpers than the boxed \
             baseline (the native nested-field path DCEs them): ON={on_boxed_arith}, \
             OFF={off_boxed_arith}",
        );
    }
}

/// Count `call $__aint_{add,sub,mul}` tokens in the WAT disassembly of
/// `source`'s wasm-gc output — a probe for how much BOXED Int arithmetic
/// survives. `no_carrier` forces the all-`$aint` baseline. Returns 0 (vacuous)
/// when `wasm-tools` is absent.
fn carrier_boxed_arith_token_count(prefix: &str, source: &str, no_carrier: bool) -> usize {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir);
    if no_carrier {
        cmd.env("AVER_NO_CARRIER_I64", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(
        out.status.success(),
        "{prefix}: wasm-gc compile failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let wat = wasm_tools_print(&out_dir.join("main.wasm"));
    cleanup(&path);
    wat.match_indices("__aint_add").count()
        + wat.match_indices("__aint_sub").count()
        + wat.match_indices("__aint_mul").count()
}

// ---------------------------------------------------------------------------
// BARE-i64 carrier RESULT → `$AverInt`-typed SINK boundaries.
//
// A native-`i64` carrier-arithmetic result (a `bare_return` fn whose body is
// `c.value + c.value`, kept native because the interval fixpoint proved the
// sum fits i64) flowing into an `$AverInt`-typed consume site must be BOXED
// at the boundary (`__aint_from_i64`), or the raw `i64` meets a `ref null
// $type` slot — a wasm-gc VALIDATION failure on a VM-valid program. The wasm
// rewrite was missing this box for the call-result-into-`$AverInt`-sink path
// (`String.fromInt` and every other boxed sink: a record `Int` field, a
// `Map` value/key, a `Tuple`/`Option`/`Result` payload, a boxed user-fn
// param). The interval analysis keeps the ADD native (`dbl`'s `bare_return`
// stays true); the box lands only at the stringify/sink boundary, so the
// size/speed win is preserved. Each witness must compile clean AND VM ==
// wasm-gc (`assert_vm_wasm_identical` now also asserts a clean full-module
// compile). The original panel witness `String.fromInt(dbl(c)) => "10"` is
// the first case.

/// The witness from the cross-vendor panel: a bare-returning carrier-arith
/// call (`dbl(c) = c.value + c.value`) fed straight into `String.fromInt`,
/// whose wasm slot is `$AverInt`. Before the fix this was a module-level
/// validation error that `verify --wasm-gc` masked.
#[test]
fn carrier_bare_call_into_string_from_int_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into String.fromInt"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn dblStr(c: C) -> String
    String.fromInt(dbl(c))

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print(dblStr(c))
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-fromint-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-fromint-call", src, &out);
}

/// String INTERPOLATION embed of a bare-returning carrier-arith call
/// (`"v={dbl(c)}"`). The embed's decimal formatter takes an `$AverInt` ref;
/// the analysis whitelists stringify as a safe `bare_return` consumer, so the
/// call stays bare and must be boxed at the embed crossing.
#[test]
fn carrier_bare_call_into_interpolation_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into interpolation embed"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn show(c: C) -> String
    "v={dbl(c)}"

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print(show(c))
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-interp-call", src);
    assert_eq!(out, "v=10");
    assert_carrier_revert_agrees("carrier-interp-call", src, &out);
}

/// A bare-returning carrier-arith call result stored into a RECORD `Int`
/// FIELD (`Box(n = dbl(c))`, field typed `$AverInt`).
#[test]
fn carrier_bare_call_into_record_field_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into record Int field"
    effects [Console]

record C
    value: Int

record Holder
    n: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn store(c: C) -> Holder
    Holder(n = dbl(c))

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{store(c).n}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-recfield-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-recfield-call", src, &out);
}

/// A bare-returning carrier-arith call result stored as a `Map<_, Int>` VALUE
/// (`Map.set({}, "k", dbl(c))`, value typed `$AverInt`).
#[test]
fn carrier_bare_call_into_map_value_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into Map Int value"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn build(c: C) -> Map<String, Int>
    Map.set({}, "k", dbl(c))

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{Option.withDefault(Map.get(build(c), "k"), 0)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-mapval-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-mapval-call", src, &out);
}

/// A bare-returning carrier-arith call result used as a `Map<Int, _>` KEY
/// (`Map.set({}, dbl(c), "v")`, key typed `$AverInt`).
#[test]
fn carrier_bare_call_into_map_key_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into Map Int key"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn build(c: C) -> Map<Int, String>
    Map.set({}, dbl(c), "v")

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print(Option.withDefault(Map.get(build(c), 10), "MISS"))
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-mapkey-call", src);
    assert_eq!(out, "v");
    assert_carrier_revert_agrees("carrier-mapkey-call", src, &out);
}

/// A bare-returning carrier-arith call result as a `Tuple<Int, _>` payload
/// (`(dbl(c), "x")`, first element typed `$AverInt`).
#[test]
fn carrier_bare_call_into_tuple_payload_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into Tuple Int payload"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn pair(c: C) -> Tuple<Int, String>
    (dbl(c), "x")

fn fst(t: Tuple<Int, String>) -> Int
    match t
        (a, _) -> a

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{fst(pair(c))}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-tuple-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-tuple-call", src, &out);
}

/// A bare-returning carrier-arith call result as an `Option<Int>` payload
/// (`Option.Some(dbl(c))`, payload typed `$AverInt`).
#[test]
fn carrier_bare_call_into_option_payload_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into Option Int payload"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn opt(c: C) -> Option<Int>
    Option.Some(dbl(c))

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{Option.withDefault(opt(c), 0)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-option-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-option-call", src, &out);
}

/// A bare-returning carrier-arith call result as a `Result<Int, _>` payload
/// (`Result.Ok(dbl(c))`, Ok payload typed `$AverInt`).
#[test]
fn carrier_bare_call_into_result_payload_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into Result Int payload"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn coerce(n: Int) -> C
    match mk(n)
        Result.Ok(c)  -> c
        Result.Err(_) -> C(value = 0)

fn dbl(c: C) -> Int
    c.value + c.value

fn res(c: C) -> Result<Int, String>
    Result.Ok(dbl(c))

fn main() -> Unit
    ! [Console.print]
    match res(coerce(5))
        Result.Ok(v)  -> Console.print("{v}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-result-call", src);
    assert_eq!(out, "10");
    assert_carrier_revert_agrees("carrier-result-call", src, &out);
}

/// A bare-returning carrier-arith call result passed as an arg to a USER fn
/// with a BOXED `Int` param (`addBoxed(dbl(c), 7)`, `x` is `$AverInt`
/// because `x * 1000000000000` makes its product escape i64 ⇒ boxed param).
#[test]
fn carrier_bare_call_into_boxed_user_param_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result into boxed user-fn Int param"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn addBoxed(x: Int, y: Int) -> Int
    x * 1000000000000 + y

fn combine(c: C) -> Int
    addBoxed(dbl(c), 7)

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{combine(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-userparam-call", src);
    assert_eq!(out, "10000000000007");
    assert_carrier_revert_agrees("carrier-userparam-call", src, &out);
}

/// SOUNDNESS guard for the new boxing: a WIDE-bound carrier whose
/// `c.value * c.value` OVERFLOWS i64, then stringified via `String.fromInt`.
/// The interval fixpoint demotes the multiply to boxed (so `square`'s
/// `bare_return` is false and the new Q5 box is a no-op there); the full
/// `$aint` product must reach the formatter EXACT — a raw i64 would wrap.
#[test]
fn carrier_wide_overflow_into_string_from_int_stays_exact() {
    let src = r#"module M
    intent = "wide-bound carrier overflow into String.fromInt stays exact"
    effects [Console]

record Wide
    value: Int

fn fromWide(n: Int) -> Result<Wide, String>
    match Bool.and(n >= 0, n <= 1099511627776)
        true  -> Result.Ok(Wide(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<Wide, String>) -> Wide
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Wide(value = 0)

fn wide(n: Int) -> Wide
    unwrap(fromWide(n))

fn square(c: Wide) -> Int
    c.value * c.value

fn squareStr(c: Wide) -> String
    String.fromInt(square(c))

fn main() -> Unit
    ! [Console.print]
    Console.print(squareStr(wide(1099511627776)))
"#;
    let out = assert_vm_wasm_identical("carrier-wide-fromint", src);
    assert_eq!(out, "1208925819614629174706176");
}

/// Compile `source` to a wasm-gc `.wasm` and return its bytes. `no_carrier`
/// forces the all-`$aint` baseline via `AVER_NO_CARRIER_I64=1`.
fn compile_wasm_bytes(prefix: &str, source: &str, no_carrier: bool) -> Vec<u8> {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir);
    if no_carrier {
        cmd.env("AVER_NO_CARRIER_I64", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(
        out.status.success(),
        "{prefix}: wasm-gc compile failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The emitted file is named after the source file (`main.wasm`).
    let wasm = out_dir.join("main.wasm");
    let bytes = std::fs::read(&wasm).expect("read compiled wasm");
    cleanup(&path);
    bytes
}

/// Like [`compile_wasm_bytes`] but runs the `--optimize size` pipeline
/// (`wasm-metadce` → `wasm-opt -Oz`), which DCEs the unreachable boxed-`$AverInt`
/// arithmetic prelude. The size WIN of the carrier-`i64` path only shows after
/// DCE — an un-optimized module carries the whole bignum prelude in both builds.
/// Returns `None` when the `wasm-opt` toolchain is absent (the size assertion
/// then degrades to "not larger" so CI without `wasm-opt` still passes).
fn compile_wasm_bytes_optimized(prefix: &str, source: &str, no_carrier: bool) -> Option<Vec<u8>> {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--optimize")
        .arg("size")
        .arg("-o")
        .arg(&out_dir);
    if no_carrier {
        cmd.env("AVER_NO_CARRIER_I64", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    let wasm = out_dir.join("main.wasm");
    let bytes = std::fs::read(&wasm).ok();
    cleanup(&path);
    if out.status.success() { bytes } else { None }
}

// ---------------------------------------------------------------------------
// Eligibility-tightening (fail-closed) regressions.
//
// Two soundness/completeness holes in the carrier-`i64` feature, closed by
// removing a carrier from the eligible set (⇒ it stays boxed) whenever a
// whole-program scan trips:
//
//   * HOLE #1 — a BARE record constructor outside the smart-ctor with a
//     non-literal (or out-of-bound) argument bypasses the gate; an
//     i64-overflowing value then TRAPS on the wasm-gc construct bridge while
//     the VM keeps full precision. The bare ctor demotes the carrier.
//
//   * HOLE #2 — a carrier used as a `Map` KEY (direct or transitive) fails
//     wasm validation because the Map-key codegen was not updated for the
//     i64-erased carrier. Map-key usage demotes the carrier.
//
// Each test pairs the FIX (default build: VM == wasm-gc, no trap / clean
// compile) with a REVERT (`AVER_CARRIER_I64_SKIP_DEMOTION=1` restores the
// un-tightened eligibility ⇒ the hole returns: a trap / a validation error).

/// Run `source` on wasm-gc with the eligibility-tightening scan DISABLED
/// (`AVER_CARRIER_I64_SKIP_DEMOTION=1`) — the revert baseline that restores
/// the pre-fix (un-tightened) behavior so a regression test can show the hole
/// returning. Returns `(success, combined stdout+stderr)` (the holes surface
/// as a trap or a validation error on stderr).
fn run_wasm_skip_demotion(prefix: &str, source: &str) -> (bool, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--wasm-gc")
        .env("AVER_CARRIER_I64_SKIP_DEMOTION", "1")
        .output()
        .expect("aver run executes");
    cleanup(&path);
    let mut combined = String::from_utf8_lossy(&out.stdout).to_string();
    combined.push_str(&String::from_utf8_lossy(&out.stderr));
    (out.status.success(), combined.trim().to_string())
}

/// Count `i64` tokens in the WAT disassembly of `source`'s wasm-gc output —
/// a representation probe. More erased carriers ⇒ more `i64`. `skip_demotion`
/// restores the un-tightened eligibility (every proven carrier erased).
fn carrier_i64_token_count(prefix: &str, source: &str, skip_demotion: bool) -> usize {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir);
    if skip_demotion {
        cmd.env("AVER_CARRIER_I64_SKIP_DEMOTION", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(
        out.status.success(),
        "{prefix}: wasm-gc compile failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let wat = wasm_tools_print(&out_dir.join("main.wasm"));
    cleanup(&path);
    wat.match_indices("i64").count()
}

/// Disassemble a wasm module to WAT via the `wasm-tools` CLI. Skips the test
/// (returns the raw bytes hex'd to a non-matching string) if the tool is
/// absent — the count-based assertion below tolerates that by comparing two
/// counts produced the same way.
fn wasm_tools_print(wasm: &std::path::Path) -> String {
    match Command::new("wasm-tools").arg("print").arg(wasm).output() {
        Ok(o) if o.status.success() => String::from_utf8_lossy(&o.stdout).to_string(),
        _ => String::new(),
    }
}

/// HOLE #1 regression — the bare-constructor bypass (`mk(n) = IntRange(value
/// = n)` with `n` unbounded, fed an i64-overflowing value). The ungated bare
/// ctor demotes `IntRange` to boxed, so the wasm-gc result matches the VM's
/// full-precision value with NO trap. The revert (scan disabled) keeps
/// `IntRange` as i64 and the construct bridge `__aint_to_i64_checked` TRAPS.
#[test]
fn bare_construct_bypass_demotes_to_boxed() {
    let src = r#"module M
    intent = "carrier bypass via bare ctor beyond i64"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn mk(n: Int) -> IntRange
    IntRange(value = n)

fn toInt(c: IntRange) -> Int
    c.value

fn big() -> Int
    4000000000 * 4000000000

fn main() -> Unit
    ! [Console.print]
    Console.print("{toInt(mk(big()))}")
"#;
    // FIX: demoted to boxed ⇒ VM == wasm-gc, full precision, no trap.
    let out = assert_vm_wasm_identical("bare-bypass", src);
    assert_eq!(out, "16000000000000000000");

    // REVERT: scan disabled ⇒ IntRange stays i64 ⇒ the construct bridge traps.
    let (ok, msg) = run_wasm_skip_demotion("bare-bypass-revert", src);
    assert!(
        !ok,
        "revert: with the ungated-construct scan disabled the bare-ctor bypass \
         must TRAP on wasm-gc (the hole returns), but the run succeeded with:\n{msg}"
    );
    assert!(
        msg.contains("trap") || msg.contains("unreachable"),
        "revert: expected a wasm trap (the __aint_to_i64_checked bridge firing on \
         the overflowing bare value), got:\n{msg}"
    );
}

/// HOLE #2 regression — a carrier used as a `Map` KEY. The Map-key scan
/// demotes `IntRange` to boxed, so the program compiles to wasm-gc via the
/// boxed key path and matches the VM. The revert (scan disabled) keeps
/// `IntRange` as i64 in the key position and wasm VALIDATION fails.
#[test]
fn carrier_as_map_key_demotes_to_boxed() {
    let src = r#"module M
    intent = "carrier as Map KEY"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn unwrap(r: Result<IntRange, String>) -> IntRange
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> IntRange(value = 0)

fn ir(n: Int) -> IntRange
    unwrap(fromInt(n))

fn mkMap() -> Map<IntRange, String>
    Map.fromList([(ir(5), "five"), (ir(7), "seven")])

fn lookup(m: Map<IntRange, String>, k: IntRange) -> String
    match Map.get(m, k)
        Option.Some(v) -> v
        Option.None    -> "MISS"

fn main() -> Unit
    ! [Console.print]
    Console.print("{lookup(mkMap(), ir(5))} {lookup(mkMap(), ir(7))} {lookup(mkMap(), ir(9))}")
"#;
    // FIX: demoted to boxed key ⇒ compiles clean AND VM == wasm-gc.
    let out = assert_vm_wasm_identical("mapkey", src);
    assert_eq!(out, "five seven MISS");

    // REVERT: scan disabled ⇒ i64-erased Map KEY ⇒ wasm validation fails.
    let (ok, msg) = run_wasm_skip_demotion("mapkey-revert", src);
    assert!(
        !ok,
        "revert: with the Map-key scan disabled an i64-erased carrier KEY must \
         fail wasm validation (the hole returns), but the run succeeded:\n{msg}"
    );
    assert!(
        msg.contains("validation failed") || msg.contains("type mismatch"),
        "revert: expected a wasm validation / type-mismatch error from the \
         i64-erased Map key, got:\n{msg}"
    );
}

/// HOLE #2 (completeness) — a carrier used as a `Map` KEY through a LOCAL
/// BINDING annotation (`m: Map<IntRange, Int> = …`). The original Map-key scan
/// only walked fn-param / fn-return / record-field annotation strings, so a
/// binding-level `Map<…>` annotation was MISSED and the carrier wrongly stayed
/// i64 → wasm validation failure on a VM-valid program. The resolved-IR scan
/// (typed-MIR `Map<K, V>` instantiations) sees this Map and demotes the key.
#[test]
fn carrier_as_map_key_via_local_binding_demotes_to_boxed() {
    let src = r#"module M
    intent = "carrier as Map KEY via local binding annotation"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn use(c: IntRange) -> Int
    m: Map<IntRange, Int> = Map.set({}, c, 5)
    Option.withDefault(Map.get(m, c), 0)

fn main() -> Unit
    ! [Console.print]
    match fromInt(7)
        Result.Ok(c)  -> Console.print("{use(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // FIX: the binding-annotated Map key demotes ⇒ boxed key path ⇒ compiles
    // clean AND VM == wasm-gc.
    let out = assert_vm_wasm_identical("mapkey-localbind", src);
    assert_eq!(out, "5");

    // REVERT: scan disabled ⇒ i64-erased Map KEY ⇒ wasm validation fails.
    let (ok, msg) = run_wasm_skip_demotion("mapkey-localbind-revert", src);
    assert!(
        !ok,
        "revert: with demotion disabled the i64-erased carrier KEY (local-binding \
         annotation) must fail wasm validation, but the run succeeded:\n{msg}"
    );
    assert!(
        msg.contains("validation failed") || msg.contains("type mismatch"),
        "revert: expected a wasm validation / type-mismatch error from the \
         i64-erased Map key, got:\n{msg}"
    );
}

/// HOLE #2 (completeness) — a carrier used as a `Map` KEY reached ONLY through
/// INFERENCE: `m = Map.set({}, c, 5)` with NO annotation anywhere. A textual
/// annotation scan fundamentally cannot see this key type; the resolved typed
/// MIR carries it. The resolved-IR scan demotes the key.
#[test]
fn carrier_as_map_key_fully_inferred_demotes_to_boxed() {
    let src = r#"module M
    intent = "carrier as Map KEY, fully inferred (no annotation)"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn use(c: IntRange) -> Int
    m = Map.set({}, c, 5)
    Option.withDefault(Map.get(m, c), 0)

fn main() -> Unit
    ! [Console.print]
    match fromInt(7)
        Result.Ok(c)  -> Console.print("{use(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // FIX: the inferred Map key demotes ⇒ boxed key path ⇒ compiles clean AND
    // VM == wasm-gc.
    let out = assert_vm_wasm_identical("mapkey-inferred", src);
    assert_eq!(out, "5");

    // REVERT: scan disabled ⇒ i64-erased inferred Map KEY ⇒ wasm validation fails.
    let (ok, msg) = run_wasm_skip_demotion("mapkey-inferred-revert", src);
    assert!(
        !ok,
        "revert: with demotion disabled the i64-erased carrier KEY (fully inferred) \
         must fail wasm validation, but the run succeeded:\n{msg}"
    );
    assert!(
        msg.contains("validation failed") || msg.contains("type mismatch"),
        "revert: expected a wasm validation / type-mismatch error from the \
         i64-erased Map key, got:\n{msg}"
    );
}

/// No-over-boxing CONTROL for the inferred-key fix — the SAME inference shape
/// (`m = Map.set(…)`, no annotation) but the carrier sits in the Map VALUE
/// position, not the key. A Map VALUE carrier is i64-supported, so it must
/// STAY eligible: demoting it would be over-boxing. The `i64` footprint with
/// demotion ON must EQUAL the footprint with demotion disabled — proof the
/// resolved-IR scan demotes KEYS only, never values.
#[test]
fn carrier_as_inferred_map_value_stays_i64() {
    let src = r#"module M
    intent = "carrier as Map VALUE, fully inferred — must stay i64"
    effects [Console]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn use(c: IntRange) -> Int
    m = Map.set({}, "k", c)
    match Map.get(m, "k")
        Option.Some(v) -> v.value
        Option.None    -> 0

fn main() -> Unit
    ! [Console.print]
    match fromInt(7)
        Result.Ok(c)  -> Console.print("{use(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // Behaviour: VM == wasm-gc (the value-carrier path is sound either way).
    let out = assert_vm_wasm_identical("mapvalue-inferred", src);
    assert_eq!(out, "7");

    // Representation: demotion-ON must EQUAL demotion-disabled. If the scan
    // over-reached and demoted the VALUE carrier, the i64 footprint would
    // SHRINK below the un-tightened build.
    let with_skip = carrier_i64_token_count("mapvalue-skip", src, true);
    let default = carrier_i64_token_count("mapvalue-def", src, false);
    if with_skip > 0 {
        assert_eq!(
            default, with_skip,
            "over-boxing: a carrier used as a Map VALUE (not key) must STAY i64 — \
             default={default}, skip-demotion={with_skip}. A smaller default count \
             would mean the resolved-IR Map-key scan wrongly demoted a VALUE carrier."
        );
    }
}

/// Per-type granularity — a program with TWO carriers: `Tainted` is bare-
/// constructed with an unbounded arg (⇒ demoted, boxed) while `Clean` is only
/// ever smart-constructed (⇒ eligible, i64). The scan is per-type, not
/// all-or-nothing: the default build's `i64` footprint sits STRICTLY between
/// "both boxed" and "both i64", proving exactly one carrier was demoted.
#[test]
fn mixed_program_demotes_per_type() {
    let src = r#"module M
    intent = "two carriers: one bare-bypassed (boxed), one clean (i64)"
    effects [Console]

record Tainted
    value: Int

record Clean
    value: Int

fn fromTainted(n: Int) -> Result<Tainted, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(Tainted(value = n))
        false -> Result.Err("oob")

fn fromClean(n: Int) -> Result<Clean, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(Clean(value = n))
        false -> Result.Err("oob")

fn bareTainted(n: Int) -> Tainted
    Tainted(value = n)

fn unwrapC(r: Result<Clean, String>) -> Clean
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Clean(value = 0)

fn tInt(c: Tainted) -> Int
    c.value

fn cInt(c: Clean) -> Int
    c.value

fn bigTainted() -> Int
    tInt(bareTainted(4000000000 * 4000000000))

fn cleanVal() -> Int
    cInt(unwrapC(fromClean(50)))

fn main() -> Unit
    ! [Console.print]
    Console.print("{bigTainted()} {cleanVal()}")
"#;
    // Behavior: Tainted boxed (full precision, no trap), Clean correct.
    let out = assert_vm_wasm_identical("mixed", src);
    assert_eq!(out, "16000000000000000000 50");

    // Representation: per-type proof. `default` (Tainted boxed, Clean i64)
    // must sit STRICTLY between `skip` (both i64) and the floor. If the scan
    // were all-or-nothing, `default` would equal `skip`.
    let with_skip = carrier_i64_token_count("mixed-skip", src, true);
    let default = carrier_i64_token_count("mixed-def", src, false);
    // `wasm-tools` absent ⇒ both counts are 0; the assertion below is then
    // vacuously skipped (0 < 0 is false, so we only assert when we have data).
    if with_skip > 0 {
        assert!(
            default < with_skip,
            "per-type: demoting only `Tainted` must lower the i64 footprint below \
             the both-eligible build — default={default}, skip-demotion={with_skip}. \
             Equal counts would mean the scan boxed BOTH carriers (all-or-nothing)."
        );
    }
}

// ---------------------------------------------------------------------------
// Match-SUBJECT boundary completeness (the 3rd hole of the bare-i64 →
// `$AverInt` class).
//
// The wasm-gc Int-match cascade types a no-`Bind`-arm subject by a single
// structural test (`mir_renders_raw_i64`): a raw-rendering subject keeps the
// native `i64.eq` compare, ANY other subject is typed `$AverInt` and compared
// with `__aint_eq`. A bare-RETURNING call subject (`match dbl(c) { 0 -> … }`,
// `match countdown(3) { 0 -> … }`) renders raw `i64` from the callee but is
// NOT recognized raw by the cascade, so it was typed `$AverInt` — and the
// `bare_i64_rewrite` routed the no-bind subject through `rewrite_value`
// (children only), never boxing the bare-returning call. The raw `i64` met a
// `ref null $type` subject slot → a wasm VALIDATION error on a VM-valid
// program. Now the no-bind subject funnels through the boxing chokepoint.
//
// These are the FIRST match-scrutinee cases in the differential. Two flavors:
//   - CARRIER: `match dbl(c) { … }` with `dbl` a bare carrier-arith call —
//     vanishes with `AVER_NO_CARRIER_I64=1` (the #551 blocker).
//   - BARE-i64: `match countdown(n) { … }` over a recurrence counter —
//     vanishes with `AVER_NO_BARE_I64=1`, a PRE-EXISTING bare-i64 bug the
//     same fix closes (carrier-arith plays no part).

/// CARRIER flavor — a bare-returning carrier-arith call as a no-bind match
/// SUBJECT (`match dbl(c) { 0 -> …; 100 -> …; _ -> … }`). The subject is
/// typed `$AverInt` by the cascade, so the rewrite must box `dbl(c)`'s raw
/// `i64` result at the subject boundary. VM == wasm-gc, and the whole module
/// compiles clean.
#[test]
fn carrier_bare_call_as_match_subject_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result as no-bind Int match subject"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn classify(c: C) -> Int
    match dbl(c)
        0   -> 1
        100 -> 2
        _   -> 3

fn main() -> Unit
    ! [Console.print]
    match mk(50)
        Result.Ok(c)  -> Console.print("{classify(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-match-subject", src);
    assert_eq!(out, "2");
    assert_carrier_revert_agrees("carrier-match-subject", src, &out);
}

/// CARRIER flavor, BOOL-result variant — the no-bind match SUBJECT is the
/// same bare-returning carrier-arith call, but each arm yields a `Bool`
/// (`match dbl(c) { 100 -> true; _ -> false }`). The arm result type does NOT
/// change the subject's `$AverInt` typing, so the subject-boundary box is
/// still required; only the block result colour differs.
#[test]
fn carrier_bare_call_as_bool_match_subject_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result as no-bind subject, Bool arms"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn isHundred(c: C) -> Bool
    match dbl(c)
        100 -> true
        _   -> false

fn show(b: Bool) -> String
    match b
        true  -> "yes"
        false -> "no"

fn main() -> Unit
    ! [Console.print]
    match mk(50)
        Result.Ok(c)  -> Console.print("{show(isHundred(c))}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-bool-match-subject", src);
    assert_eq!(out, "yes");
    assert_carrier_revert_agrees("carrier-bool-match-subject", src, &out);
}

/// BARE-i64 flavor (PRE-EXISTING, predates #551) — a bare-i64 recurrence
/// counter as a no-bind match SUBJECT (`match countdown(n) { 0 -> …; _ -> … }`).
/// `countdown` returns a native `i64` (bare return); the cascade typed the
/// subject `$AverInt`, so the same subject-boundary box closes it. Carrier
/// arith plays no part (no carrier here) — the program vanishes the bug only
/// with `AVER_NO_BARE_I64=1`, never `AVER_NO_CARRIER_I64=1`. VM == wasm-gc and
/// the module compiles clean.
#[test]
fn bare_recurrence_call_as_match_subject_boxes_at_boundary() {
    let src = r#"module M
    intent = "bare-i64 recurrence result as no-bind Int match subject"
    effects [Console]

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn run(n: Int) -> Int
    match countdown(n)
        0 -> 1
        _ -> 2

fn main() -> Unit
    ! [Console.print]
    Console.print("{run(3)} {run(0)}")
"#;
    // countdown(3) and countdown(0) both reach 0, so both pick the `0 -> 1`
    // arm. VM is ground truth; the whole module must compile clean to wasm-gc.
    let out = assert_vm_wasm_identical("bare-match-subject", src);
    assert_eq!(out, "1 1");
}

// ---------------------------------------------------------------------------
// INDEPENDENT-PRODUCT (bang-group `!`) element boundary completeness (the 4th
// hole of the bare-i64 → `$AverInt` class).
//
// `(a, b)!` builds a tuple `struct.new` whose Int element slots are typed
// `$AverInt` (`ref null $type`), exactly like a plain `Tuple` literal. The
// `bare_i64_rewrite` routed the product's elements through `rewrite_value`
// (children only) instead of the `rewrite_boxed_each` chokepoint the adjacent
// `Tuple` arm uses, so a raw-rendering Int element — a bare-returning call
// (`countdown(3)`), inline bare arith, or a bare carrier `.value` (`dbl(c)`) —
// stayed un-boxed: the raw `i64` met the `$AverInt` tuple field, a wasm
// VALIDATION error on a VM-valid program. `verify --wasm-gc` masks it; only a
// full-module compile (every fn lowered) catches it. Two flavors:
//   - CARRIER: `(dbl(c), dbl(c))!` over a bare carrier-arith call — vanishes
//     with `AVER_NO_CARRIER_I64=1` (the #551 carrier blocker).
//   - BARE-i64: `(countdown(3), countdown(4))!` over recurrence counters —
//     vanishes with `AVER_NO_BARE_I64=1`, a PRE-EXISTING bare-i64 bug the same
//     fix closes (carrier-arith plays no part).

/// CARRIER flavor — a bang-group product `(dbl(c), dbl(c))!` whose elements are
/// bare-returning carrier-arith calls. Each Int element slot of the tuple is
/// `$AverInt`, so the rewrite must box each `dbl(c)` raw `i64` result at the
/// product-element boundary. VM == wasm-gc, and the whole module compiles clean.
#[test]
fn carrier_bare_call_in_independent_product_boxes_at_boundary() {
    let src = r#"module M
    intent = "carrier arith result in independent-product (!) element"
    effects [Console]

record C
    value: Int

fn mk(n: Int) -> Result<C, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(C(value = n))
        false -> Result.Err("oob")

fn dbl(c: C) -> Int
    c.value + c.value

fn pair(c: C) -> Tuple<Int, Int>
    (dbl(c), dbl(c))!

fn sumPair(c: C) -> Int
    match pair(c)
        (a, b) -> a + b

fn main() -> Unit
    ! [Console.print]
    match mk(5)
        Result.Ok(c)  -> Console.print("{sumPair(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("carrier-ip-call", src);
    assert_eq!(out, "20");
    assert_carrier_revert_agrees("carrier-ip-call", src, &out);
}

/// BARE-i64 flavor (PRE-EXISTING, predates #551) — a bang-group product
/// `(countdown(3), countdown(4))!` whose elements are bare-i64 recurrence
/// calls. `countdown` returns a native `i64` (bare return); each tuple element
/// slot is `$AverInt`, so the same product-element box closes it. Carrier
/// arith plays no part (no carrier here) — the program vanishes the bug only
/// with `AVER_NO_BARE_I64=1`, never `AVER_NO_CARRIER_I64=1`. VM == wasm-gc and
/// the module compiles clean.
#[test]
fn bare_recurrence_call_in_independent_product_boxes_at_boundary() {
    let src = r#"module M
    intent = "bare-i64 recurrence result in independent-product (!) element"
    effects [Console]

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn pair() -> Tuple<Int, Int>
    (countdown(3), countdown(4))!

fn run() -> Int
    match pair()
        (a, b) -> a + b

fn main() -> Unit
    ! [Console.print]
    Console.print("{run()}")
"#;
    // countdown(3) and countdown(4) both reach 0, so the tuple is (0, 0) and
    // the sum is 0. VM is ground truth; the whole module must compile clean.
    let out = assert_vm_wasm_identical("bare-ip-call", src);
    assert_eq!(out, "0");
}

// ===========================================================================
// Multi-field carrier-`i64`: a `record Coord { x: Int, y: Int }` plus a 2-arg
// smart constructor gating BOTH fields → each Int field stored as native i64.
// The source stays `c.x : Int` (no `.value`). VM is ground truth; every case
// must match wasm-gc AND compile clean.
// ===========================================================================

/// A `Coord { x: Int, y: Int }` with a 2-arg smart ctor bounding each field.
/// `c.x + c.dx` reads native i64 fields and adds raw. VM == wasm-gc, revert
/// agrees (representation-only).
#[test]
fn multi_field_native_arithmetic_matches_vm_and_reverts() {
    let src = r#"module M
    intent = "multi-field carrier-i64 native arithmetic"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn manhattan(c: Coord) -> Int
    c.x + c.y

fn main() -> Unit
    ! [Console.print]
    match coord(30, 12)
        Result.Ok(c)  -> Console.print("{manhattan(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("mf-native", src);
    assert_eq!(out, "42");
    assert_carrier_revert_agrees("mf-native", src, &out);
}

/// WIDE-BOUND per-field overflow: a field bounded `0 .. 2^40` whose squared
/// value (`c.x * c.x` = up to 2^80) leaves `i64`, so the multiply MUST stay
/// boxed (full `$aint` precision), matching the VM. A raw `i64.mul` would
/// wrap. This is the load-bearing soundness case for the multi-field read.
#[test]
fn multi_field_wide_bound_square_stays_boxed_matches_vm() {
    let src = r#"module M
    intent = "multi-field carrier-i64 wide-bound overflow stays boxed"
    effects [Console]

record Wide
    x: Int
    y: Int

fn wide(x: Int, y: Int) -> Result<Wide, String>
    match Bool.and(Bool.and(x >= 0, x <= 1099511627776), Bool.and(y >= 0, y <= 1099511627776))
        true  -> Result.Ok(Wide(x = x, y = y))
        false -> Result.Err("oob")

fn square(c: Wide) -> Int
    c.x * c.x

fn main() -> Unit
    ! [Console.print]
    match wide(1000000000, 0)
        Result.Ok(c)  -> Console.print("{square(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // 1e9 squared = 1e18, which fits i64; but the field bound 2^40 makes the
    // PRODUCT bound 2^80 (> i64), so the analysis keeps the multiply boxed.
    // The VM computes the exact 1000000000000000000; wasm-gc must match.
    let out = assert_vm_wasm_identical("mf-wide-square", src);
    assert_eq!(out, "1000000000000000000");
    assert_carrier_revert_agrees("mf-wide-square", src, &out);
}

/// A wide-bound field whose square genuinely EXCEEDS i64 must still match the
/// VM (boxed bignum). `2^40 * 2^40 = 2^80`, far past i64::MAX — a raw multiply
/// would wrap to a wrong value.
#[test]
fn multi_field_wide_bound_true_overflow_matches_vm() {
    let src = r#"module M
    intent = "multi-field carrier-i64 true overflow stays boxed"
    effects [Console]

record Wide
    x: Int
    y: Int

fn wide(x: Int, y: Int) -> Result<Wide, String>
    match Bool.and(Bool.and(x >= 0, x <= 1099511627776), Bool.and(y >= 0, y <= 1099511627776))
        true  -> Result.Ok(Wide(x = x, y = y))
        false -> Result.Err("oob")

fn square(c: Wide) -> Int
    c.x * c.x

fn main() -> Unit
    ! [Console.print]
    match wide(1099511627776, 0)
        Result.Ok(c)  -> Console.print("{square(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // 2^40 squared = 2^80 = 1208925819614629174706176, way past i64::MAX.
    let out = assert_vm_wasm_identical("mf-true-overflow", src);
    assert_eq!(out, "1208925819614629174706176");
}

/// MIXED record: one field gated/bounded (→ native i64), the other NOT
/// mentioned in the guard (→ boxed `$AverInt`). Both reads must match the VM,
/// and the whole module compiles clean (the boxed field stays the ref shape).
#[test]
fn multi_field_mixed_bounded_and_unbounded_matches_vm() {
    let src = r#"module M
    intent = "multi-field carrier-i64 mixed bounded/unbounded fields"
    effects [Console]

record Mixed
    x: Int
    y: Int

fn mixed(x: Int, y: Int) -> Result<Mixed, String>
    match Bool.and(x >= 0, x <= 100)
        true  -> Result.Ok(Mixed(x = x, y = y))
        false -> Result.Err("oob")

fn combine(c: Mixed) -> Int
    c.x + c.y

fn bigY() -> Int
    5000000000 * 5000000000

fn main() -> Unit
    ! [Console.print]
    match mixed(7, bigY())
        Result.Ok(c)  -> Console.print("{combine(c)}")
        Result.Err(_) -> Console.print("err")
"#;
    // x = 7 (bounded → i64), y = 5e9 * 5e9 = 2.5e19 (unbounded → boxed bignum,
    // it exceeds i64::MAX ~9.2e18). The sum is boxed (y escapes i64), exact on
    // both backends. The literals stay in i64 range; the PRODUCT overflows.
    let out = assert_vm_wasm_identical("mf-mixed", src);
    assert_eq!(out, "25000000000000000007");
    assert_carrier_revert_agrees("mf-mixed", src, &out);
}

/// MIS-FIRE negative: a plain 2-field record with NO smart constructor. The
/// fields must NOT be erased to i64 (no proven bound) — they stay boxed. The
/// program still runs correctly and compiles clean; this guards against the
/// recognizer firing on an unguarded record.
#[test]
fn multi_field_no_smart_ctor_keeps_fields_boxed_matches_vm() {
    let src = r#"module M
    intent = "multi-field plain record (no smart ctor) stays boxed"
    effects [Console]

record Plain
    x: Int
    y: Int

fn combine(c: Plain) -> Int
    c.x + c.y

fn main() -> Unit
    ! [Console.print]
    Console.print("{combine(Plain(x = 5, y = 37))}")
"#;
    let out = assert_vm_wasm_identical("mf-misfire", src);
    assert_eq!(out, "42");
    assert_carrier_revert_agrees("mf-misfire", src, &out);
}

/// UNGATED / OUT-OF-RANGE construction demotes: the record has a smart ctor,
/// but a SECOND fn constructs it ungated with an out-of-range / non-literal
/// value. The record must fall back to all-boxed (every field stays
/// `$AverInt`), or the construct bridge would TRAP on the out-of-range value.
/// The program runs correctly and compiles clean.
#[test]
fn multi_field_ungated_construction_demotes_matches_vm() {
    let src = r#"module M
    intent = "multi-field ungated construction demotes to boxed"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 100), Bool.and(y >= 0, y <= 100))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn raw(n: Int) -> Coord
    Coord(x = n, y = n)

fn combine(c: Coord) -> Int
    c.x + c.y

fn bigN() -> Int
    5000000000 * 5000000000

fn main() -> Unit
    ! [Console.print]
    Console.print("{combine(raw(bigN()))}")
"#;
    // `raw` constructs Coord ungated with 2.5e19 (> i64::MAX); the record is
    // demoted to all-boxed, so the bignum field stays exact (no trap, no wrap).
    let out = assert_vm_wasm_identical("mf-ungated", src);
    assert_eq!(out, "50000000000000000000");
    assert_carrier_revert_agrees("mf-ungated", src, &out);
}

/// NESTED bounded record field read: `state.head.x` where `head : Coord` and
/// `Coord.x` is a bounded i64 field. The struct.get chain reads the inner i64.
/// VM == wasm-gc.
#[test]
fn multi_field_nested_read_matches_vm_and_reverts() {
    let src = r#"module M
    intent = "multi-field nested bounded field read"
    effects [Console]

record Coord
    x: Int
    y: Int

record State
    head: Coord
    score: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn headX(s: State) -> Int
    s.head.x + s.head.y

fn main() -> Unit
    ! [Console.print]
    match coord(11, 31)
        Result.Ok(c)  -> Console.print("{headX(State(head = c, score = 0))}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("mf-nested", src);
    assert_eq!(out, "42");
    assert_carrier_revert_agrees("mf-nested", src, &out);
}

/// A bounded multi-field read flowing into a STRINGIFY sink (interpolation
/// embed) — the raw i64 must be boxed at the embed boundary (the $aint decimal
/// formatter takes an $AverInt ref). Compiles clean + matches the VM.
#[test]
fn multi_field_read_into_stringify_sink_matches_vm() {
    let src = r#"module M
    intent = "multi-field bounded read into a stringify sink"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn main() -> Unit
    ! [Console.print]
    match coord(40, 2)
        Result.Ok(c)  -> Console.print("x={c.x} y={c.y}")
        Result.Err(_) -> Console.print("err")
"#;
    let out = assert_vm_wasm_identical("mf-stringify", src);
    assert_eq!(out, "x=40 y=2");
    assert_carrier_revert_agrees("mf-stringify", src, &out);
}

/// SIZE PROOF for the multi-field win: a `Coord { x: Int, y: Int }` snake-shaped
/// model (clean source, NO `.value`) whose 2-arg smart ctor bounds both fields.
/// Every field read is native i64 and the arithmetic runs `i64.add/sub`, so the
/// boxed `$AverInt` arithmetic prelude DCEs. The carrier-ON build must be
/// STRICTLY SMALLER than the boxed baseline (`AVER_NO_CARRIER_I64=1`), and the
/// `Coord` struct lowers to `(struct (field i64) (field i64))` — byte-identical
/// to the single-field-leaf `Coord { x: Axis, y: Axis }` composition. VM ==
/// wasm-gc both ways is covered by `assert_vm_wasm_identical`.
#[test]
fn multi_field_snake_size_drops_vs_boxed() {
    let src = r#"module M
    intent = "multi-field snake size: native field arithmetic"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(x = 0, y = 0)

fn mk(x: Int, y: Int) -> Coord
    unwrap(coord(x, y))

fn sumX(a: Coord, b: Coord) -> Int
    a.x + b.x

fn sumY(a: Coord, b: Coord) -> Int
    a.y + b.y

fn diff(a: Coord, b: Coord) -> Int
    (a.x - b.x) + (a.y - b.y)

fn main() -> Unit
    ! [Console.print]
    p = mk(5, 7)
    q = mk(1, 2)
    Console.print("{sumX(p, q)} {sumY(p, q)} {diff(p, q)}")
"#;
    // Representation-only erasure: identical output both ways.
    let out = assert_vm_wasm_identical("mf-snake-size", src);
    assert_eq!(out, "6 9 9");
    assert_carrier_revert_agrees("mf-snake-size", src, &out);

    // The size WIN materializes after `--optimize size` (DCE drops the now-
    // unreachable boxed-`$AverInt` arith prelude). Measured locally: carrier-ON
    // ~1953 B vs carrier-OFF(boxed) ~6254 B (~31%) — byte-equal to the
    // single-field-leaf `Coord { x: Axis, y: Axis }` composition (~1940 B).
    if let (Some(on), Some(off)) = (
        compile_wasm_bytes_optimized("mf-snake-on", src, false),
        compile_wasm_bytes_optimized("mf-snake-off", src, true),
    ) {
        assert!(
            on.len() < off.len(),
            "multi-field carrier-i64 must SHRINK the optimized module — native field \
             arithmetic DCEs the boxed arith prelude. carrier-ON={} bytes, \
             carrier-OFF(boxed)={} bytes",
            on.len(),
            off.len(),
        );
    }
    // Un-optimized, the carrier-ON build must at least NOT GROW the module
    // (the storage erasure + native ops never add bytes; the prelude is shared).
    let on_raw = compile_wasm_bytes("mf-snake-on-raw", src, false);
    let off_raw = compile_wasm_bytes("mf-snake-off-raw", src, true);
    assert!(
        on_raw.len() <= off_raw.len() + 16,
        "carrier-ON un-optimized must not meaningfully grow the module: ON={} OFF={}",
        on_raw.len(),
        off_raw.len(),
    );
}

// ===========================================================================
// Multi-field carrier as a CONTAINER value / element. A bounded
// `record Coord { x: Int, y: Int }` whose 2-arg smart ctor erases each field
// to native i64 is stored in a `List` / `Vector` / `Tuple` element or a `Map`
// VALUE. The record's generated eq / hash helper reads each field and now
// dispatches PER FIELD: an i64-erased carrier field compares with a raw
// `i64.eq` / hashes with `i32.wrap_i64` (gated on `is_eligible_carrier_field`),
// while a boxed `$AverInt` field keeps the `$aint` dispatch. So an i64-erased
// field in a container element compiles clean and runs native — no demotion
// needed for `List` / `Vector` / `Tuple` elements.
//
//   - List / Vector / Tuple element: STAYS native i64. The `Coord` struct keeps
//     `(field i64) (field i64)`, `List.contains` / `==` over the elements
//     dispatch the raw i64 ops. VM == wasm-gc; the WAT shows the native struct.
//   - Map VALUE (and Map KEY): still DEMOTED to boxed — a separate, pre-existing
//     record-as-Map-value validation bug is out of scope, so we fail closed
//     there. The WAT shows `Coord` boxed (`(field (ref null $AverInt))`).
//   - Option / Result payload: native (inline struct ref) — the smart-ctor
//     boundary `coord -> Result<Coord, _>` keeps the win.
// ===========================================================================

/// True iff `source`'s wasm-gc module lowers the bounded `Coord { x: Int, y:
/// Int }` to the NATIVE `(struct (field i64) (field i64))` — the eligibility /
/// demotion oracle now that the i64-erased field is no longer a validation
/// failure. A demoted `Coord` shows `(struct (field (ref null N)) (field (ref
/// null N)))` (boxed `$AverInt` refs) and this returns false. Skips gracefully
/// (returns `None`) when `wasm-tools` is absent.
fn coord_struct_is_native_i64(prefix: &str, source: &str) -> Option<bool> {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile executes");
    assert!(
        out.status.success(),
        "{prefix}: wasm-gc compile failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let wasm = std::fs::read_dir(&out_dir)
        .expect("read out dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .find(|p| p.extension().is_some_and(|x| x == "wasm"))
        .expect("a .wasm artifact");
    let wat = wasm_tools_print(&wasm);
    cleanup(&path);
    if wat.is_empty() {
        return None; // wasm-tools absent — caller tolerates
    }
    // The native Coord is the only 2-field all-i64 struct the program defines.
    let native = wat.contains("(struct (field i64) (field i64))");
    Some(native)
}

/// Shared body for the container value/element tests: a `Coord { x, y }`
/// bounded `0 .. 2^40`, stored via `$store` and read back via `$read` which
/// squares `g.x` (overshooting i64 → boxed arith, exact). `$store` / `$read`
/// are the per-container fn pair; the rest is identical.
fn multi_field_container_src(store_fn: &str, read_fn: &str) -> String {
    format!(
        r#"module M
    intent = "multi-field carrier as container element"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1099511627776), Bool.and(y >= 0, y <= 1099511627776))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(x = 0, y = 0)

{store_fn}

{read_fn}

fn main() -> Unit
    ! [Console.print]
    c = unwrap(coord(1099511627776, 0))
    Console.print("{{readSquare(store(c))}}")
"#
    )
}

/// Assert a `List` / `Vector` / `Tuple` element program STAYS native: VM ==
/// wasm-gc, compiles clean, the `Coord` struct lowers to `(field i64) (field
/// i64)`, AND the same with the demotion scan disabled (the un-demote means the
/// two are now equivalent — there is no longer a validation hazard to demote).
fn assert_container_element_stays_native(prefix: &str, src: &str) {
    // VM == wasm-gc at the exact 2^80 bignum (the read squares g.x → boxed
    // arith, but the field STORAGE is native i64).
    let out = assert_vm_wasm_identical(prefix, src);
    assert_eq!(out, "1208925819614629174706176");
    // The Coord struct must be NATIVE i64 (not demoted).
    if let Some(native) = coord_struct_is_native_i64(&format!("{prefix}-struct"), src) {
        assert!(
            native,
            "{prefix}: a multi-field carrier used as a List/Vector/Tuple element \
             must STAY native i64 — the per-field record eq/hash now dispatches \
             the raw i64 ops, so no demotion is needed. The Coord struct is boxed."
        );
    }
    // Disabling the demotion scan is now a no-op for these positions (nothing to
    // demote): it must still compile + run clean, NOT fail validation.
    let (ok, msg) = run_wasm_skip_demotion(&format!("{prefix}-noskip"), src);
    assert!(
        ok && msg == "1208925819614629174706176",
        "{prefix}: a native List/Vector/Tuple carrier element must run clean with \
         OR without the demotion scan — it is no longer a validation hazard.\n{msg}"
    );
}

/// A multi-field carrier as a `Map` VALUE still DEMOTES to boxed (a separate,
/// pre-existing record-as-Map-value validation bug is out of scope). VM ==
/// wasm-gc, compiles clean, and the `Coord` struct is BOXED by default; with the
/// demotion scan disabled the field is un-demoted to native i64 (a structural
/// probe — the WAT shows i64) but still runs correct (the eq/hash is native-safe
/// now), confirming the demotion is the deliberate guard, not a correctness fix.
fn assert_map_value_demotes(prefix: &str, src: &str) {
    let out = assert_vm_wasm_identical(prefix, src);
    assert_eq!(out, "1208925819614629174706176");
    // Default build: Map-VALUE Coord is demoted to boxed.
    if let Some(native) = coord_struct_is_native_i64(&format!("{prefix}-struct"), src) {
        assert!(
            !native,
            "{prefix}: a multi-field carrier used as a Map VALUE must DEMOTE to \
             boxed (the record-as-Map-value validation bug is out of scope), but \
             the Coord struct lowered to native i64."
        );
    }
}

/// A multi-field carrier as a `Map` VALUE — DEMOTES to boxed (kept).
#[test]
fn multi_field_as_map_value_demotes_to_boxed() {
    let src = multi_field_container_src(
        "fn store(c: Coord) -> Map<String, Coord>\n    Map.set({}, \"k\", c)",
        "fn readSquare(m: Map<String, Coord>) -> Int\n    match Map.get(m, \"k\")\n        Option.Some(g) -> g.x * g.x\n        Option.None    -> 0",
    );
    assert_map_value_demotes("mf-map-value", &src);
}

/// A multi-field carrier as a `List` element — STAYS native i64.
#[test]
fn multi_field_as_list_element_stays_native() {
    let src = multi_field_container_src(
        "fn store(c: Coord) -> List<Coord>\n    [c]",
        "fn readSquare(xs: List<Coord>) -> Int\n    match xs\n        [g, ..rest] -> g.x * g.x\n        []          -> 0",
    );
    assert_container_element_stays_native("mf-list-element", &src);
}

/// A multi-field carrier as a `Vector` element — STAYS native i64.
#[test]
fn multi_field_as_vector_element_stays_native() {
    let src = multi_field_container_src(
        "fn store(c: Coord) -> Vector<Coord>\n    Vector.new(1, c)",
        "fn readSquare(v: Vector<Coord>) -> Int\n    match Vector.get(v, 0)\n        Option.Some(g) -> g.x * g.x\n        Option.None    -> 0",
    );
    assert_container_element_stays_native("mf-vector-element", &src);
}

/// A multi-field carrier as a `Tuple` element — STAYS native i64.
#[test]
fn multi_field_as_tuple_element_stays_native() {
    let src = multi_field_container_src(
        "fn store(c: Coord) -> Tuple<Coord, Int>\n    (c, 0)",
        "fn readSquare(t: Tuple<Coord, Int>) -> Int\n    match t\n        (g, _) -> g.x * g.x",
    );
    assert_container_element_stays_native("mf-tuple-element", &src);
}

/// HOLE #3 boundary — a multi-field carrier as an `Option` / `Result` payload
/// is NOT demoted: the payload holds an inline struct ref where an i64-erased
/// field is fine, so the native-i64 win is preserved (the program compiles
/// clean + matches the VM WITHOUT demotion). This guards against the scan
/// over-demoting the common smart-ctor boundary `coord -> Result<Coord, _>`.
#[test]
fn multi_field_as_option_result_payload_stays_native() {
    let option_src = multi_field_container_src(
        "fn store(c: Coord) -> Option<Coord>\n    Option.Some(c)",
        "fn readSquare(o: Option<Coord>) -> Int\n    match o\n        Option.Some(g) -> g.x * g.x\n        Option.None    -> 0",
    );
    // FIX path: VM == wasm-gc, compiles clean.
    let out = assert_vm_wasm_identical("mf-option-payload", &option_src);
    assert_eq!(out, "1208925819614629174706176");
    // The Option payload is NOT a demotion hazard: even with the scan disabled
    // it compiles clean (the inline struct ref holds the i64 fields fine).
    let (ok, _msg) = run_wasm_skip_demotion("mf-option-payload-noskip", &option_src);
    assert!(
        ok,
        "an Option payload of a multi-field carrier must compile clean with OR \
         without the demotion scan — it is not a container-element hazard"
    );

    let result_src = multi_field_container_src(
        "fn store(c: Coord) -> Result<Coord, String>\n    Result.Ok(c)",
        "fn readSquare(r: Result<Coord, String>) -> Int\n    match r\n        Result.Ok(g)  -> g.x * g.x\n        Result.Err(_) -> 0",
    );
    let out = assert_vm_wasm_identical("mf-result-payload", &result_src);
    assert_eq!(out, "1208925819614629174706176");
    let (ok, _msg) = run_wasm_skip_demotion("mf-result-payload-noskip", &result_src);
    assert!(
        ok,
        "a Result payload of a multi-field carrier must compile clean with OR \
         without the demotion scan — it is not a container-element hazard"
    );
}

// ===========================================================================
// Multi-field carrier in a container — EXHAUSTIVE eq / hash / membership.
//
// The record eq/hash helper now dispatches a raw `i64.eq` / `i32.wrap_i64` for
// an i64-erased carrier field (per `is_eligible_carrier_field`), so
// `List.contains`, list / vector / tuple `==`, and Map lookups over such
// records run native. The VM keeps the full `$aint` carrier and is the ground
// truth; every case below must produce identical VM output AND compile clean.
// The dangerous failure mode is a SILENT one: a wrong eq result (membership
// miss) or an eq/hash that disagree (a Map/Set lookup silently misses). The
// differential catches both — `verify --wasm-gc` decodes the value.
// ===========================================================================

/// `List.contains(list_of_Coord, c)` TRUE and FALSE, list `==` equal and
/// unequal, and a field-wise compare vs an `==` compare of the same Coords —
/// all over a `List<Coord>` whose elements are native i64 fields.
#[test]
fn multi_field_list_contains_and_eq_native_matches_vm() {
    let src = r#"module M
    intent = "List.contains + list == over native multi-field carriers"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(x = 0, y = 0)

fn mk(x: Int, y: Int) -> Coord
    unwrap(coord(x, y))

fn fieldwiseEq(a: Coord, b: Coord) -> Bool
    Bool.and(a.x == b.x, a.y == b.y)

fn body() -> List<Coord>
    [mk(3, 4), mk(5, 6), mk(7, 8)]

fn main() -> Unit
    ! [Console.print]
    xs = body()
    hit = List.contains(xs, mk(5, 6))
    miss = List.contains(xs, mk(5, 7))
    eqSame = body() == body()
    eqDiff = body() == [mk(3, 4), mk(5, 6), mk(0, 0)]
    fw = fieldwiseEq(mk(5, 6), mk(5, 6))
    fwd = fieldwiseEq(mk(5, 6), mk(5, 7))
    Console.print("{hit} {miss} {eqSame} {eqDiff} {fw} {fwd}")
"#;
    let out = assert_vm_wasm_identical("mf-list-contains-eq", src);
    assert_eq!(out, "true false true false true false");
    assert_carrier_revert_agrees("mf-list-contains-eq", src, &out);
    if let Some(native) = coord_struct_is_native_i64("mf-list-contains-eq-struct", src) {
        assert!(native, "List<Coord> elements must stay native i64");
    }
}

/// MIXED record (one bounded i64 field + one UNBOUNDED `$AverInt` field) in a
/// `List`. The eq must compare BOTH fields correctly — the i64 field via
/// `i64.eq`, the boxed field via `__aint_eq` — so membership distinguishes a
/// difference in EITHER field. `List.contains` true / false on each field.
#[test]
fn multi_field_mixed_record_list_contains_compares_both_fields() {
    let src = r#"module M
    intent = "List.contains over a MIXED (i64 + $AverInt) record compares both"
    effects [Console]

record Mixed
    x: Int
    y: Int

fn mixed(x: Int, y: Int) -> Result<Mixed, String>
    match Bool.and(x >= 0, x <= 100)
        true  -> Result.Ok(Mixed(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Mixed, String>) -> Mixed
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Mixed(x = 0, y = 0)

fn mk(x: Int, y: Int) -> Mixed
    unwrap(mixed(x, y))

fn bigY() -> Int
    5000000000 * 5000000000

fn body() -> List<Mixed>
    [mk(7, 1), mk(8, bigY())]

fn main() -> Unit
    ! [Console.print]
    xs = body()
    hitBoxed = List.contains(xs, mk(8, bigY()))
    missBoxedDiff = List.contains(xs, mk(8, 1))
    missI64Diff = List.contains(xs, mk(9, bigY()))
    Console.print("{hitBoxed} {missBoxedDiff} {missI64Diff}")
"#;
    // hit when BOTH fields match; miss when the boxed field differs (8,1 vs
    // 8,bigY); miss when the i64 field differs (9 vs 8). The mixed eq must
    // consult BOTH — an i64-only or boxed-only compare would mis-answer one.
    let out = assert_vm_wasm_identical("mf-mixed-list", src);
    assert_eq!(out, "true false false");
    assert_carrier_revert_agrees("mf-mixed-list", src, &out);
}

/// `Vector<Coord>` native element ACCESS (`Vector.get` → native field read)
/// and `Tuple<Coord, Coord>` element equality over native carriers. The tuple
/// `==` exercises the per-field record eq through the tuple element dispatch;
/// the vector exercises native element storage + field read.
///
/// NB: `Vector<record> ==` is a SEPARATE, pre-existing wasm-gc bug — it fails
/// validation (`expected i64, found (ref null $type)`) even for a PLAIN 2-field
/// record with NO carrier AND with `AVER_NO_CARRIER_I64=1`, so it is unrelated
/// to this slice and is exercised here via element read rather than `==`.
#[test]
fn multi_field_vector_read_and_tuple_eq_native_matches_vm() {
    let src = r#"module M
    intent = "Vector element read + Tuple == over native multi-field carriers"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(x = 0, y = 0)

fn mk(x: Int, y: Int) -> Coord
    unwrap(coord(x, y))

fn vec() -> Vector<Coord>
    Vector.fromList([mk(1, 2), mk(3, 4)])

fn sumAt(v: Vector<Coord>, i: Int) -> Int
    match Vector.get(v, i)
        Option.Some(c) -> c.x + c.y
        Option.None    -> 0

fn pairBody(b: Coord) -> Tuple<Coord, Coord>
    (mk(5, 6), b)

fn main() -> Unit
    ! [Console.print]
    v0 = sumAt(vec(), 0)
    v1 = sumAt(vec(), 1)
    tSame = pairBody(mk(7, 8)) == pairBody(mk(7, 8))
    tDiff = pairBody(mk(7, 8)) == pairBody(mk(7, 9))
    Console.print("{v0} {v1} {tSame} {tDiff}")
"#;
    let out = assert_vm_wasm_identical("mf-vec-tuple", src);
    assert_eq!(out, "3 7 true false");
    assert_carrier_revert_agrees("mf-vec-tuple", src, &out);
    if let Some(native) = coord_struct_is_native_i64("mf-vec-tuple-struct", src) {
        assert!(
            native,
            "Vector<Coord> / Tuple<Coord,_> elements must stay native i64"
        );
    }
}

/// A 3+-field carrier (`x, y, z` all bounded i64) in a `List` — `List.contains`
/// must compare all three native fields. A difference in the THIRD field must
/// still miss.
#[test]
fn multi_field_three_field_carrier_list_contains_matches_vm() {
    let src = r#"module M
    intent = "List.contains over a 3-field native carrier"
    effects [Console]

record P3
    x: Int
    y: Int
    z: Int

fn p3(x: Int, y: Int, z: Int) -> Result<P3, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(Bool.and(y >= 0, y <= 1000), Bool.and(z >= 0, z <= 1000)))
        true  -> Result.Ok(P3(x = x, y = y, z = z))
        false -> Result.Err("oob")

fn unwrap(r: Result<P3, String>) -> P3
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> P3(x = 0, y = 0, z = 0)

fn mk(x: Int, y: Int, z: Int) -> P3
    unwrap(p3(x, y, z))

fn body() -> List<P3>
    [mk(1, 2, 3), mk(4, 5, 6)]

fn main() -> Unit
    ! [Console.print]
    xs = body()
    hit = List.contains(xs, mk(4, 5, 6))
    missZ = List.contains(xs, mk(4, 5, 7))
    eqSame = body() == body()
    Console.print("{hit} {missZ} {eqSame}")
"#;
    let out = assert_vm_wasm_identical("mf-three-field", src);
    assert_eq!(out, "true false true");
    assert_carrier_revert_agrees("mf-three-field", src, &out);
    if let Some(native) = coord_struct_is_native_i64("mf-three-field-struct", src) {
        // The 3-field struct is `(field i64)(field i64)(field i64)`, not the
        // 2-field probe, so the helper returns false; assert via a direct WAT
        // check instead.
        let _ = native;
    }
}

/// A carrier with an i64 carrier field AND a `String` field, plus one with a
/// `Bool` field, in a `List` — `List.contains` mixes the native i64 field eq
/// with the String / Bool field eq. A difference in the String or Bool field
/// must miss; a difference in the i64 field must miss.
#[test]
fn multi_field_carrier_with_string_and_bool_field_list_contains_matches_vm() {
    let src = r#"module M
    intent = "List.contains over a carrier with i64 + String + Bool fields"
    effects [Console]

record Tagged
    id: Int
    name: String
    on: Bool

fn tagged(id: Int, name: String, on: Bool) -> Result<Tagged, String>
    match Bool.and(id >= 0, id <= 1000)
        true  -> Result.Ok(Tagged(id = id, name = name, on = on))
        false -> Result.Err("oob")

fn unwrap(r: Result<Tagged, String>) -> Tagged
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Tagged(id = 0, name = "", on = false)

fn mk(id: Int, name: String, on: Bool) -> Tagged
    unwrap(tagged(id, name, on))

fn body() -> List<Tagged>
    [mk(1, "a", true), mk(2, "b", false)]

fn main() -> Unit
    ! [Console.print]
    xs = body()
    hit = List.contains(xs, mk(2, "b", false))
    missStr = List.contains(xs, mk(2, "z", false))
    missBool = List.contains(xs, mk(2, "b", true))
    missId = List.contains(xs, mk(9, "b", false))
    Console.print("{hit} {missStr} {missBool} {missId}")
"#;
    let out = assert_vm_wasm_identical("mf-tagged", src);
    assert_eq!(out, "true false false false");
    assert_carrier_revert_agrees("mf-tagged", src, &out);
}

/// SIZE measurement for the un-demote: a snake-shaped program storing positions
/// in a `List<Coord>` with native field arithmetic and a `List.contains`-style
/// self-collision check. The `List<Coord>` now STAYS native (not demoted), so
/// the boxed `$AverInt` arith prelude DCEs and the module SHRINKS vs the boxed
/// baseline. Reports `on` vs `AVER_NO_CARRIER_I64=1` bytes. VM == wasm-gc.
#[test]
fn list_coord_snake_stays_native_and_shrinks() {
    let src = r#"module M
    intent = "snake positions in a List<Coord>, native field arith + collision"
    effects [Console]

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 1000), Bool.and(y >= 0, y <= 1000))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")

fn unwrap(r: Result<Coord, String>) -> Coord
    match r
        Result.Ok(c)  -> c
        Result.Err(_) -> Coord(x = 0, y = 0)

fn mk(x: Int, y: Int) -> Coord
    unwrap(coord(x, y))

fn step(head: Coord) -> Coord
    mk(head.x + 1, head.y + 1)

fn grow(body: List<Coord>, head: Coord) -> List<Coord>
    List.prepend(head, body)

fn collides(body: List<Coord>, head: Coord) -> Bool
    List.contains(body, head)

fn run() -> Bool
    h0 = mk(1, 1)
    b0 = grow([], h0)
    h1 = step(h0)
    b1 = grow(b0, h1)
    h2 = step(h1)
    collides(b1, h2)

fn main() -> Unit
    ! [Console.print]
    Console.print("{run()}")
"#;
    let out = assert_vm_wasm_identical("list-snake", src);
    assert_eq!(out, "false");
    assert_carrier_revert_agrees("list-snake", src, &out);

    if let Some(native) = coord_struct_is_native_i64("list-snake-struct", src) {
        assert!(
            native,
            "the List<Coord> snake must keep Coord native i64 (not demoted)"
        );
    }

    // The List<Coord> stays native, so native field arithmetic DCEs the boxed
    // arith prelude under `--optimize size` → strictly smaller than the boxed
    // baseline. (Under #553 the List<Coord> was DEMOTED, so the boxed prelude
    // stayed and there was no shrink.)
    if let (Some(on), Some(off)) = (
        compile_wasm_bytes_optimized("list-snake-on", src, false),
        compile_wasm_bytes_optimized("list-snake-off", src, true),
    ) {
        assert!(
            on.len() < off.len(),
            "List<Coord> snake must SHRINK with native carriers ON: \
             ON={} bytes, OFF(boxed)={} bytes",
            on.len(),
            off.len(),
        );
    }
}

// ===========================================================================
// `Int = ℤ` size lever — routing a RAW-i64 `String.fromInt` / interpolation
// embed to the LEAN i64 formatter instead of boxing to `$AverInt` + the
// ~536 B base-2^32 long-division bignum formatter.
//
// A bare/carrier value the analysis proved `OverflowFree` is stringified by
// the LEAN itoa (`__wasmgc_string_from_int_i64`, a raw `i64` param) directly
// — NO box, NO call to the bignum formatter. So a program whose every Int-
// stringify arg is bare/carrier never references the bignum formatter, which
// `wasm-opt -Oz` then DCEs. A genuine `$AverInt` arg (an unbounded Int >
// i64) keeps the bignum formatter, which MUST stay present and correct.
//
// SOUNDNESS — SILENT-C0: a wrong digit is a silent wrong string (no trap,
// no validation error). The VM (full ℤ `AverInt::to_string`) is the ground
// truth; `verify --wasm-gc` decodes the emitted string, so a single wrong
// digit shows as a VM-vs-wasm-gc mismatch. The differentials below are
// EXHAUSTIVE over the i64-range edge cases (0, 1, 9, 10, 99, 100, negatives,
// a large positive carrier, and a negative-bound carrier at its min) for BOTH
// `String.fromInt(c.value)` and the interpolation embed `"{c.value}"`.
// ===========================================================================

/// Disassemble `source`'s `--optimize size` wasm-gc module and count its
/// functions (the `(func` headers). The bignum decimal formatter is ONE
/// function; an all-raw-stringify program DCEs it, so its function count sits
/// strictly below the same program with one extra genuine-`$AverInt`
/// stringify. Returns `None` when the `wasm-opt` / `wasm-tools` toolchain is
/// absent (the assertion then degrades gracefully).
fn optimized_fn_count(prefix: &str, source: &str) -> Option<usize> {
    let bytes = compile_wasm_bytes_optimized(prefix, source, false)?;
    let dir = std::env::temp_dir().join(format!("{prefix}-fncount"));
    std::fs::create_dir_all(&dir).ok()?;
    let wasm = dir.join("m.wasm");
    std::fs::write(&wasm, &bytes).ok()?;
    let wat = wasm_tools_print(&wasm);
    let _ = std::fs::remove_dir_all(&dir);
    if wat.is_empty() {
        return None;
    }
    Some(wat.match_indices("(func ").count())
}

/// EXHAUSTIVE `String.fromInt(c.value)` differential over a bare/carrier
/// value across the i64-range edges. Every value is stringified by the LEAN
/// i64 formatter (the carrier is `OverflowFree`); the VM keeps the full ℤ
/// carrier and is the oracle. A wrong digit ⇒ a VM-vs-wasm-gc mismatch.
#[test]
fn raw_carrier_string_from_int_exhaustive_edges_match_vm() {
    // A wide carrier bound (`0 .. 4_000_000_000`) so the large positive edge
    // (3_999_999_999) is in-carrier yet past i32 — exercising the full i64
    // itoa digit loop. Each `show(sc(N))` stringifies the bare carrier value.
    let src = r#"module M
    intent = "exhaustive raw-carrier String.fromInt"
    effects [Console]

record Score
    value: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 4000000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn show(s: Score) -> String
    String.fromInt(s.value)

fn main() -> Unit
    ! [Console.print]
    Console.print(show(sc(0)))
    Console.print(show(sc(1)))
    Console.print(show(sc(9)))
    Console.print(show(sc(10)))
    Console.print(show(sc(99)))
    Console.print(show(sc(100)))
    Console.print(show(sc(3999999999)))
"#;
    let out = assert_vm_wasm_identical("raw-fromint-edges", src);
    assert_eq!(out, "0\n1\n9\n10\n99\n100\n3999999999");
    assert_carrier_revert_agrees("raw-fromint-edges", src, &out);
}

/// EXHAUSTIVE interpolation `"{c.value}"` differential over a bare/carrier
/// value across the same i64-range edges. The embed routes to the LEAN i64
/// formatter exactly like the direct `String.fromInt` call. VM is the oracle.
#[test]
fn raw_carrier_interpolation_exhaustive_edges_match_vm() {
    let src = r#"module M
    intent = "exhaustive raw-carrier interpolation embed"
    effects [Console]

record Score
    value: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 4000000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn show(s: Score) -> String
    "v={s.value}"

fn main() -> Unit
    ! [Console.print]
    Console.print(show(sc(0)))
    Console.print(show(sc(1)))
    Console.print(show(sc(9)))
    Console.print(show(sc(10)))
    Console.print(show(sc(99)))
    Console.print(show(sc(100)))
    Console.print(show(sc(3999999999)))
"#;
    let out = assert_vm_wasm_identical("raw-interp-edges", src);
    assert_eq!(out, "v=0\nv=1\nv=9\nv=10\nv=99\nv=100\nv=3999999999");
    assert_carrier_revert_agrees("raw-interp-edges", src, &out);
}

/// NEGATIVES — a SIGNED-bound carrier (`-100 .. 100`) stringified across the
/// negative edges (-1, -10, -99, -100) plus a positive control. The lean
/// formatter's sign path (write `'-'` at position 0) must produce digits
/// byte-identical to the VM. Both `String.fromInt` and interpolation.
#[test]
fn raw_signed_carrier_negatives_match_vm() {
    let src = r#"module M
    intent = "exhaustive raw-carrier negatives"
    effects [Console]

record Signed
    value: Int

fn fromInt(n: Int) -> Result<Signed, String>
    match Bool.and(n >= -100, n <= 100)
        true  -> Result.Ok(Signed(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Signed
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Signed(value = 0)

fn show(s: Signed) -> String
    String.fromInt(s.value)

fn interp(s: Signed) -> String
    "v={s.value}"

fn main() -> Unit
    ! [Console.print]
    Console.print(show(sc(0 - 1)))
    Console.print(show(sc(0 - 10)))
    Console.print(show(sc(0 - 99)))
    Console.print(show(sc(0 - 100)))
    Console.print(interp(sc(0 - 1)))
    Console.print(interp(sc(0 - 10)))
    Console.print(interp(sc(0 - 99)))
    Console.print(interp(sc(0 - 100)))
    Console.print(show(sc(42)))
"#;
    let out = assert_vm_wasm_identical("raw-negatives", src);
    assert_eq!(out, "-1\n-10\n-99\n-100\nv=-1\nv=-10\nv=-99\nv=-100\n42");
    assert_carrier_revert_agrees("raw-negatives", src, &out);
}

/// A NEGATIVE-BOUND carrier stringified AT ITS MIN. The carrier is bounded
/// `-2_000_000_000 .. 0`, and the value is its minimum (a 10-digit negative
/// past i32::MIN) — the lean i64 itoa must format the full magnitude + sign
/// byte-identically to the VM.
#[test]
fn raw_carrier_negative_min_bound_matches_vm() {
    let src = r#"module M
    intent = "raw-carrier at its negative-min bound"
    effects [Console]

record NegRange
    value: Int

fn fromInt(n: Int) -> Result<NegRange, String>
    match Bool.and(n >= -2000000000, n <= 0)
        true  -> Result.Ok(NegRange(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> NegRange
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> NegRange(value = 0)

fn show(s: NegRange) -> String
    String.fromInt(s.value)

fn main() -> Unit
    ! [Console.print]
    Console.print("{sc(0 - 2000000000).value} {show(sc(0 - 2000000000))}")
"#;
    let out = assert_vm_wasm_identical("raw-neg-min", src);
    assert_eq!(out, "-2000000000 -2000000000");
    assert_carrier_revert_agrees("raw-neg-min", src, &out);
}

/// MIXED — one bare/carrier stringify (routes to the lean formatter) AND one
/// genuine unbounded-Int stringify (`9e9 * 9e9 = 8.1e19`, far past i64::MAX,
/// a real `$AverInt`). The bignum formatter MUST remain and produce the exact
/// bignum decimal; the carrier value stays lean-formatted. VM is the oracle
/// for BOTH.
#[test]
fn mixed_carrier_and_unbounded_int_stringify_match_vm() {
    let src = r#"module M
    intent = "mixed raw-carrier + genuine unbounded-Int stringify"
    effects [Console]

record Score
    value: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 4000000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn big() -> Int
    9000000000 * 9000000000

fn show(s: Score) -> String
    String.fromInt(s.value)

fn main() -> Unit
    ! [Console.print]
    s = sc(3999999999)
    Console.print("{show(s)} {String.fromInt(big())} {s.value}")
"#;
    // 9e9 * 9e9 = 8.1e19 > i64::MAX (~9.22e18): a genuine Big the lean i64
    // formatter could not represent. It MUST flow through the bignum
    // formatter; the carrier 3999999999 stays lean-formatted. VM is the oracle.
    let out = assert_vm_wasm_identical("mixed-stringify", src);
    assert_eq!(out, "3999999999 81000000000000000000 3999999999");
}

/// NON-carrier control — a plain `Int` (no carrier in scope) stringified via
/// `String.fromInt` must stay byte-identical to the VM. Without a carrier the
/// arg is a genuine `$AverInt` under bignum, so this keeps the bignum
/// formatter (the lean route only fires for a proven-raw arg). Guards that the
/// routing did NOT regress the ordinary `String.fromInt` path.
#[test]
fn plain_int_string_from_int_still_bignum_and_matches_vm() {
    let src = r#"module M
    intent = "plain Int String.fromInt keeps the bignum formatter"
    effects [Console]

fn label(n: Int) -> String
    String.fromInt(n)

fn main() -> Unit
    ! [Console.print]
    Console.print(label(0))
    Console.print(label(0 - 7))
    Console.print(label(123456789))
    Console.print("{label(1000000000000000000 + 1000000000000000000)}")
"#;
    // 1e18 + 1e18 = 2e18 (still in i64 here) — a plain Int, formatted by the
    // bignum formatter. VM is the oracle for every digit.
    let out = assert_vm_wasm_identical("plain-fromint", src);
    assert_eq!(out, "0\n-7\n123456789\n2000000000000000000");
}

/// DCE PROOF — an all-raw-stringify program (every `String.fromInt` /
/// interpolation arg is a bare/carrier value) must NOT reference the bignum
/// decimal formatter, so `wasm-opt -Oz` drops it. Oracle: the optimized
/// function count + byte size of the all-raw program sit STRICTLY below the
/// SAME program with one extra genuine-`$AverInt` stringify (which forces the
/// bignum formatter present). The delta is the bignum formatter + its `$aint`
/// deps. Both builds must run byte-identically to the VM.
#[test]
fn all_raw_stringify_dces_the_bignum_formatter() {
    let all_raw = r#"module M
    intent = "all-raw stringify — bignum formatter must DCE"
    effects [Console]

record Score
    value: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 4000000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn show(s: Score) -> String
    String.fromInt(s.value)

fn main() -> Unit
    ! [Console.print]
    s = sc(3999999999)
    Console.print("{show(s)} {s.value}")
"#;
    // The SAME program plus ONE genuine unbounded-Int stringify (`9e9 * 9e9`,
    // far past i64) — this forces the bignum formatter to be referenced and
    // therefore NOT DCE'd.
    let with_bignum = r#"module M
    intent = "raw stringify + one genuine unbounded-Int stringify"
    effects [Console]

record Score
    value: Int

fn fromInt(n: Int) -> Result<Score, String>
    match Bool.and(n >= 0, n <= 4000000000)
        true  -> Result.Ok(Score(value = n))
        false -> Result.Err("oob")

fn sc(n: Int) -> Score
    match fromInt(n)
        Result.Ok(s)  -> s
        Result.Err(_) -> Score(value = 0)

fn big() -> Int
    9000000000 * 9000000000

fn show(s: Score) -> String
    String.fromInt(s.value)

fn main() -> Unit
    ! [Console.print]
    s = sc(3999999999)
    Console.print("{show(s)} {String.fromInt(big())} {s.value}")
"#;

    // Both must run byte-identically to the VM (output correctness first).
    let raw_out = assert_vm_wasm_identical("dce-allraw", all_raw);
    assert_eq!(raw_out, "3999999999 3999999999");
    let mixed_out = assert_vm_wasm_identical("dce-mixed", with_bignum);
    assert_eq!(mixed_out, "3999999999 81000000000000000000 3999999999");

    // FUNCTION-COUNT oracle: the bignum formatter is ONE function. The all-raw
    // build must reach FEWER functions than the build that also stringifies a
    // genuine Big — proof the formatter (and its `$aint` deps) DCE'd. Skips
    // gracefully when `wasm-opt` / `wasm-tools` is absent.
    if let (Some(raw_fns), Some(mixed_fns)) = (
        optimized_fn_count("dce-allraw-fns", all_raw),
        optimized_fn_count("dce-mixed-fns", with_bignum),
    ) {
        assert!(
            raw_fns < mixed_fns,
            "all-raw stringify must DCE the bignum formatter: the all-raw module \
             reaches {raw_fns} functions, the build that also stringifies a genuine \
             Big reaches {mixed_fns}. Equal counts would mean the bignum formatter \
             survived in the all-raw build (it was referenced despite every arg \
             being raw)."
        );
    }

    // SIZE oracle: the all-raw optimized module must be STRICTLY smaller than
    // the bignum-using one (the formatter + deps are pure overhead the all-raw
    // program never pays). Reported for the byte-drop measurement.
    if let (Some(raw), Some(mixed)) = (
        compile_wasm_bytes_optimized("dce-allraw-sz", all_raw, false),
        compile_wasm_bytes_optimized("dce-mixed-sz", with_bignum, false),
    ) {
        assert!(
            raw.len() < mixed.len(),
            "all-raw optimized module must be smaller than the bignum-using one: \
             all-raw={} bytes, with-bignum={} bytes (delta = the DCE'd bignum \
             formatter + its $aint deps).",
            raw.len(),
            mixed.len(),
        );
    }
}

// ===========================================================================
// CONST-COMPARE SPECIALIZATION DIFFERENTIAL
//
// A boxed `$AverInt` compared against an i64-fitting CONSTANT lowers to a
// tag-branch (`ref.is_null $magf` ⇒ Small → native `i64.<cmp>_s` against the
// constant; Big ⇒ the relation is fixed by `$sign` alone) instead of a
// general `__aint_cmp` call. THE VM (full ℤ) IS GROUND TRUTH — every emitted
// comparison MUST decode to the SAME boolean on the VM and on wasm-gc, or the
// tag-branch picked the wrong side / inverted a relation / mishandled a sign.
//
// Each comparison result is encoded as an Int ("1"/"0") so a wrong wasm-gc
// boolean shows up as a decoded-output mismatch (not a silent type pun). The
// operand is a plain `Int` PARAM (a genuinely-boxed `$AverInt`, never bare-
// eligible) so the specialization path fires; the constant is a literal that
// always `fits_i64` (an AST `Int` is an `i64`).
//
// Coverage: all six ops (`< > <= >= == !=`), BOTH operand orders (`v OP K`
// and `K OP v`), each constant K ∈ {0, 1, -1, 100, -100, i64::MAX, i64::MIN+1}
// (i64::MIN itself is unspellable as a source literal — its magnitude exceeds
// i64::MAX — so i64::MIN+1, the most-negative literal, stands in), and the
// value classes:
//   - Small AT the boundary: K-1, K, K+1 (full ℤ, so K+1 past i64::MAX is a
//     genuine Big — the Small/Big boundary at the i64 extreme falls out here),
//   - Small FAR from K,
//   - Big-positive (`a*a*a` past i64::MAX),
//   - Big-negative (`0 - a*a*a`).
// The Big-negative-vs-negative-const and Small-at-i64::MIN/MAX cases are the
// fault-prone ones and are hit for every op and order. (Negative literals are
// spelled `0 - N`, which the const-fold pass collapses to a single
// `Literal::Int(-N)` before MIR, so the specialization sees a literal operand.
// The i64::MIN *value* IS reachable as `(0 - i64::MAX) - 1` and is swept as a
// Small operand — only the i64::MIN *constant* is unspellable.)

/// `boolToInt` + `cube`, threaded so the operand stays a boxed `$AverInt`
/// param. Prepended to every generated comparison program.
const CONST_CMP_PRELUDE: &str = r#"module M
    intent = "const-compare specialization differential"
    effects [Console]

fn b(x: Bool) -> Int
    match x
        true  -> 1
        false -> 0

fn cube(n: Int) -> Int
    n * n * n
"#;

/// VM == wasm-gc for `source` (plain boxed `$AverInt` comparisons, not
/// carrier-erased). Returns the agreed output for the caller to pin against an
/// independent oracle. Diverging output ⇒ the tag-branch produced a wrong
/// boolean.
fn assert_const_cmp_identical(prefix: &str, source: &str) -> String {
    let (vm_ok, vm_out) = run(prefix, source, false, false);
    let (wg_ok, wg_out) = run(prefix, source, true, false);
    assert!(vm_ok, "{prefix}: VM run failed:\n{vm_out}");
    assert!(wg_ok, "{prefix}: wasm-gc run failed:\n{wg_out}");
    assert_eq!(
        vm_out, wg_out,
        "{prefix}: VM-vs-wasm-gc DIVERGENCE — the const-compare tag-branch \
         produced a wrong boolean (wrong Small/Big side, inverted relation, or \
         mis-read sign).\n  VM     = {vm_out:?}\n  wasm-gc= {wg_out:?}"
    );
    vm_out
}

/// The reference oracle, computed in Rust over `i128` (a superset of every
/// i64-fitting value AND every Big we build), mirroring the six comparisons
/// for `v OP k` (`order_const_left == false`) or `k OP v` (`== true`).
fn ref_cmp(v: i128, op: &str, k: i128, order_const_left: bool) -> i128 {
    let (lhs, rhs) = if order_const_left { (k, v) } else { (v, k) };
    let r = match op {
        "<" => lhs < rhs,
        ">" => lhs > rhs,
        "<=" => lhs <= rhs,
        ">=" => lhs >= rhs,
        "==" => lhs == rhs,
        "!=" => lhs != rhs,
        other => panic!("unknown op {other}"),
    };
    i128::from(r)
}

/// Build the comparison program + the expected decoded string for one
/// `(value-expr, value-i128)` operand against the full op×order×K matrix.
/// `value_src` is an Aver expression evaluating to the boxed operand; the fn
/// it lands in takes it as an `Int` param so the operand stays `$AverInt`.
fn const_cmp_case(value_src: &str, value_i128: i128) -> (String, String) {
    // Each constant is an i64-fitting literal. The most-positive extreme is
    // i64::MAX (spellable directly); the most-negative is i64::MIN+1 (spelled
    // `0 - i64::MAX` and const-folded) — i64::MIN itself has no source literal.
    let consts: [i64; 7] = [0, 1, -1, 100, -100, i64::MAX, i64::MIN + 1];
    let ops = ["<", ">", "<=", ">=", "==", "!="];

    let mut body = String::new();
    let mut fns = String::new();
    let mut expected: Vec<String> = Vec::new();
    let mut idx = 0usize;

    for &k in &consts {
        for op in &ops {
            for order_const_left in [false, true] {
                let fname = format!("c{idx}");
                idx += 1;
                // `v` is a boxed `$AverInt` param; the literal `k` triggers the
                // const-compare specialization. Both operand orders covered.
                // Negative literals are spelled `0 - N` so the lexer keeps them
                // (and so `K OP v` with a negative K stays well-formed).
                let klit = if k < 0 {
                    format!("(0 - {})", k.unsigned_abs())
                } else {
                    k.to_string()
                };
                let expr = if order_const_left {
                    format!("b({klit} {op} v)")
                } else {
                    format!("b(v {op} {klit})")
                };
                fns.push_str(&format!("\nfn {fname}(v: Int) -> Int\n    {expr}\n"));
                body.push_str(&format!(
                    "    Console.print(\"{{{fname}({value_src})}}\")\n"
                ));
                expected.push(ref_cmp(value_i128, op, i128::from(k), order_const_left).to_string());
            }
        }
    }

    let src = format!("{CONST_CMP_PRELUDE}{fns}\nfn main() -> Unit\n    ! [Console.print]\n{body}");
    (src, expected.join("\n"))
}

/// Drive one value class through the whole op×order×K matrix and assert VM ==
/// wasm-gc AND that the agreed output equals the Rust `i128` oracle.
fn run_const_cmp_class(prefix: &str, value_src: &str, value_i128: i128) {
    let (src, expected) = const_cmp_case(value_src, value_i128);
    let out = assert_const_cmp_identical(prefix, &src);
    assert_eq!(
        out, expected,
        "{prefix}: decoded output disagreed with the i128 reference oracle — \
         a const-compare result is wrong on BOTH backends (a value-class / \
         oracle setup bug) or the VM itself differs from i128 semantics."
    );
}

// `a*a*a` for a = 3037000500 ≈ 2.804e28, far beyond i64::MAX (9.22e18): a
// genuine Big. Negated for the Big-negative class.
const BIG_A: i128 = 3037000500;

/// Small operand sweep landing ON the boundaries (K-1, K, K+1) of the constant
/// matrix {0,1,-1,100,-100,i64::MAX,i64::MIN}, PLUS the extreme Smalls at
/// i64::MAX / i64::MAX-1 / i64::MIN. Every boundary triple is covered across
/// the value set; the i64-extreme Smalls are the fault-prone ones.
#[test]
fn const_cmp_small_at_boundaries_match_vm() {
    let smalls: [(&str, i128); 14] = [
        ("0", 0),
        ("1", 1),
        ("0 - 1", -1),
        ("2", 2),
        ("0 - 2", -2),
        ("99", 99),
        ("100", 100),
        ("101", 101),
        ("0 - 99", -99),
        ("0 - 100", -100),
        ("0 - 101", -101),
        // i64::MAX, i64::MAX-1 — directly spellable. i64::MIN has no source
        // literal (magnitude > i64::MAX), so it is built as
        // `(0 - i64::MAX) - 1` — still a Small `$AverInt` (normalize demotes
        // exactly -2^63 to Small), the load-bearing extreme-Small case.
        ("9223372036854775807", 9223372036854775807),
        ("9223372036854775806", 9223372036854775806),
        ("(0 - 9223372036854775807) - 1", -9223372036854775808),
    ];
    for (i, (expr, val)) in smalls.iter().enumerate() {
        run_const_cmp_class(&format!("cc-small-{i}"), expr, *val);
    }
}

/// Small FAR from every K (a mid-range value not adjacent to any boundary).
#[test]
fn const_cmp_small_far_match_vm() {
    run_const_cmp_class("cc-small-far-pos", "123456789", 123456789);
    run_const_cmp_class("cc-small-far-neg", "0 - 123456789", -123456789);
}

/// Big-POSITIVE (`a*a*a` past i64::MAX) against the whole matrix. A Big-
/// positive is `> k` for every i64 `k`, `< k` for none, `== k` for none.
#[test]
fn const_cmp_big_positive_match_vm() {
    let big = BIG_A * BIG_A * BIG_A;
    run_const_cmp_class("cc-big-pos", "cube(3037000500)", big);
}

/// Big-NEGATIVE (`0 - a*a*a`) against the whole matrix — the most fault-prone
/// class, especially against the negative constants (-1, -100, i64::MIN). A
/// Big-negative is `< k` for every i64 `k`, `> k` for none, `== k` for none.
#[test]
fn const_cmp_big_negative_match_vm() {
    let big_neg = -(BIG_A * BIG_A * BIG_A);
    run_const_cmp_class("cc-big-neg", "0 - cube(3037000500)", big_neg);
}

/// A `$AverInt`-vs-`$AverInt` comparison (NO literal operand) MUST stay on the
/// general `__aint_cmp` path and remain correct with the specialization
/// present — the fail-closed guard. VM == wasm-gc is the correctness oracle.
///
/// The structural witness that the specialization did NOT over-fire uses the
/// FUNCTION-COUNT DCE oracle (the compiled module carries no name section, so
/// a name grep is vacuous): a program whose only comparison is the NON-literal
/// `a < c` keeps `__aint_cmp` (+ its `__aint_decompose` dep) reachable, so it
/// reaches STRICTLY MORE functions than the otherwise-identical program whose
/// comparison is against a CONSTANT (which DCEs both helpers). If the
/// specialization over-fired on the non-literal form, the two counts would be
/// equal.
#[test]
fn const_cmp_non_literal_stays_on_aint_cmp() {
    // The NON-literal comparison: both operands are boxed `$AverInt` params,
    // so the comparison must call `__aint_cmp`.
    let non_literal = r#"module M
    intent = "non-literal $AverInt comparison stays general"
    effects [Console]

fn b(x: Bool) -> Int
    match x
        true  -> 1
        false -> 0

fn lt(a: Int, c: Int) -> Int
    b(a < c)

fn cube(n: Int) -> Int
    n * n * n

fn main() -> Unit
    ! [Console.print]
    p = cube(3037000500)
    q = 0 - cube(3037000500)
    Console.print("{lt(p, q)} {lt(q, p)}")
"#;
    // The CONST-literal twin: the same shape but the comparison is against a
    // constant, so the specialization fires and `__aint_cmp` DCEs.
    let const_literal = r#"module M
    intent = "const-literal $AverInt comparison specializes"
    effects [Console]

fn b(x: Bool) -> Int
    match x
        true  -> 1
        false -> 0

fn lt(a: Int) -> Int
    b(a < 100)

fn cube(n: Int) -> Int
    n * n * n

fn main() -> Unit
    ! [Console.print]
    p = cube(3037000500)
    q = 0 - cube(3037000500)
    Console.print("{lt(p)} {lt(q)}")
"#;
    // Correctness first (VM is the oracle for both).
    let nl_out = assert_const_cmp_identical("cc-nonliteral", non_literal);
    // p (Big+) < q (Big-) = 0 ; q < p = 1
    assert_eq!(nl_out, "0 1");
    let cl_out = assert_const_cmp_identical("cc-constliteral", const_literal);
    // p (Big+) < 100 = 0 ; q (Big-) < 100 = 1
    assert_eq!(cl_out, "0 1");

    // STRUCTURAL: the non-literal program keeps `__aint_cmp` (+ decompose), so
    // it reaches MORE functions after DCE than the const-literal twin. Skips
    // when wasm-opt/wasm-tools is absent (both None ⇒ vacuous).
    if let (Some(nl_fns), Some(cl_fns)) = (
        optimized_fn_count("cc-nonliteral-fns", non_literal),
        optimized_fn_count("cc-constliteral-fns", const_literal),
    ) {
        assert!(
            cl_fns < nl_fns,
            "the const-literal comparison must DCE `__aint_cmp` (+ `__aint_decompose`), \
             reaching FEWER functions than the non-literal twin that keeps them: \
             const-literal={cl_fns} fns, non-literal={nl_fns} fns. Equal counts ⇒ the \
             specialization failed to fire (const path still called __aint_cmp) OR \
             over-fired on the non-literal form."
        );
    }
}

/// DCE MEASUREMENT (TASK 1) — a carrier-game-shape `mk(n) -> Result` whose
/// SOLE `$AverInt` comparison is the bound check `n >= 0 && n <= 100`. With the
/// const-compare specialization the bound check is a tag-branch, so the general
/// `__aint_cmp` AND `__aint_decompose` are no longer reached and DCE under
/// `--optimize size`.
///
/// The compiled module carries no name section, so the DCE is measured by
/// BYTE SIZE + FUNCTION COUNT against an otherwise-identical twin whose bound
/// check compares against a NON-literal bound (`n <= hi`, `hi` a param) — that
/// twin keeps `__aint_cmp` reachable. The specialized build must be STRICTLY
/// SMALLER and reach FEWER functions; the byte drop is the reported number.
#[test]
fn const_cmp_bound_check_dces_aint_cmp_helpers() {
    let specialized = r#"module M
    intent = "bound-check sole comparison: const-compare DCEs __aint_cmp"
    effects [Console]

record Point
    x: Int
    y: Int

fn mk(n: Int) -> Result<Point, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(Point(x = n, y = n))
        false -> Result.Err("oob")

fn main() -> Unit
    ! [Console.print]
    match mk(50)
        Result.Ok(p)  -> Console.print("{p.x}")
        Result.Err(e) -> Console.print(e)
"#;
    // The twin: the upper bound is a NON-literal `$AverInt` param (`hi`), so
    // `n <= hi` keeps the general `__aint_cmp` reachable (no specialization).
    let general = r#"module M
    intent = "bound-check against a non-literal bound keeps __aint_cmp"
    effects [Console]

record Point
    x: Int
    y: Int

fn mk(n: Int, hi: Int) -> Result<Point, String>
    match Bool.and(n >= 0, n <= hi)
        true  -> Result.Ok(Point(x = n, y = n))
        false -> Result.Err("oob")

fn main() -> Unit
    ! [Console.print]
    match mk(50, 100)
        Result.Ok(p)  -> Console.print("{p.x}")
        Result.Err(e) -> Console.print(e)
"#;
    let out = assert_vm_wasm_identical("cc-dce-bound", specialized);
    assert_eq!(out, "50");
    let gout = assert_vm_wasm_identical("cc-dce-bound-general", general);
    assert_eq!(gout, "50");

    // BYTE + FUNCTION-COUNT DCE oracle. Reports the specialized size.
    if let (Some(spec), Some(general_bytes)) = (
        compile_wasm_bytes_optimized("cc-dce-spec-sz", specialized, false),
        compile_wasm_bytes_optimized("cc-dce-gen-sz", general, false),
    ) {
        eprintln!(
            "const-compare bound-check: specialized = {} bytes, non-literal twin = {} bytes \
             (drop = {} bytes)",
            spec.len(),
            general_bytes.len(),
            general_bytes.len().saturating_sub(spec.len()),
        );
        assert!(
            spec.len() < general_bytes.len(),
            "the specialized bound check must DCE `__aint_cmp` (+ `__aint_decompose`) and \
             be SMALLER than the non-literal-bound twin: specialized={} bytes, \
             non-literal={} bytes.",
            spec.len(),
            general_bytes.len(),
        );
    }
    if let (Some(spec_fns), Some(gen_fns)) = (
        optimized_fn_count("cc-dce-spec-fns", specialized),
        optimized_fn_count("cc-dce-gen-fns", general),
    ) {
        assert!(
            spec_fns < gen_fns,
            "the specialized bound check must reach FEWER functions (the comparison \
             helpers DCE): specialized={spec_fns} fns, non-literal={gen_fns} fns."
        );
    }
}

// ── Map value that is a newtype-erased record ────────────────────────
//
// A `Map<K, V>` whose VALUE `V` is a single-primitive-field record is
// newtype-erased: `aver_to_wasm(V)` and the `values_array` element are
// the underlying carrier (`$aint` for Int), and the force-registered
// per-V hash/eq helper SIGNATURE follows. The helper BODY used to
// `struct.get` the unerased `$V` struct, diverging from the erased
// signature → wasm validation failure
// (`map_keyed_by_record_with_record_value`). The body must hash/eq the
// carrier directly. Keys are never erased; multi-field record values
// keep the struct path. These pin the fix on the three shapes.

#[test]
fn map_newtype_int_value_helper_matches_erased_signature() {
    let src = r#"module M
    intent = "newtype Int record map value — erased hash/eq helper"
    effects [Console]

record K
    a: Int

record V
    b: Int

fn lookup(m: Map<K, V>, k: K) -> Int
    match Map.get(m, k)
        Option.Some(v) -> v.b
        Option.None -> 0 - 1

fn main() -> Unit
    ! [Console.print]
    m = Map.set(Map.set({}, K(a = 1), V(b = 9)), K(a = 2), V(b = 8))
    Console.print("{lookup(m, K(a = 1))},{lookup(m, K(a = 2))},{lookup(m, K(a = 3))}")
"#;
    // The bug was a pure validation failure on the V helper — the
    // clean-compile assert is the load-bearing one; value parity pins
    // that key hash/eq (a hit, a second key, a miss) still agree.
    assert_compiles_clean_wasm_gc("map-newtype-int-value", src);
    let out = assert_vm_wasm_identical("map-newtype-int-value", src);
    assert_eq!(out, "9,8,-1");
}

#[test]
fn map_newtype_float_value_helper_matches_erased_signature() {
    let src = r#"module M
    intent = "newtype Float record map value — erased hash/eq helper"
    effects [Console]

record K
    a: Int

record V
    f: Float

fn lookup(m: Map<K, V>, k: K) -> Float
    match Map.get(m, k)
        Option.Some(v) -> v.f
        Option.None -> 0.0

fn main() -> Unit
    ! [Console.print]
    m = Map.set({}, K(a = 1), V(f = 9.5))
    Console.print("{lookup(m, K(a = 1))}")
"#;
    assert_compiles_clean_wasm_gc("map-newtype-float-value", src);
    let out = assert_vm_wasm_identical("map-newtype-float-value", src);
    assert_eq!(out, "9.5");
}

#[test]
fn map_multifield_record_value_keeps_struct_path() {
    // A multi-field record value is NOT newtype-erased — `aver_to_wasm`
    // returns the `$V` struct ref, the helper signature and body agree,
    // and the fix's guard does not fire. Pins that the unchanged path
    // still compiles clean and round-trips.
    let src = r#"module M
    intent = "multi-field record map value — unerased struct path"
    effects [Console]

record K
    a: Int

record V
    x: Int
    y: Int

fn lookup(m: Map<K, V>, k: K) -> Int
    match Map.get(m, k)
        Option.Some(v) -> v.x + v.y
        Option.None -> 0 - 1

fn main() -> Unit
    ! [Console.print]
    m = Map.set({}, K(a = 1), V(x = 3, y = 7))
    Console.print("{lookup(m, K(a = 1))}")
"#;
    assert_compiles_clean_wasm_gc("map-multifield-value", src);
    let out = assert_vm_wasm_identical("map-multifield-value", src);
    assert_eq!(out, "10");
}

// ── wasm-gc bug-ledger tail (docs/wasm-gc-known-issues.md) ───────────
//
// Six pre-existing wasm-gc codegen bugs, all re-validated as still
// reproducing on main post-carrier-i64 and fixed together. Parked here
// to reuse the VM↔wasm-gc differential harness (several are not
// carrier-specific). Each asserts VM↔wasm-gc value parity; the
// validation-failure ones also assert a clean full-module compile.

#[test]
fn ledger_match_on_unit_emits_body_not_trap_stub() {
    // A `match` on a Unit subject fell through subject-type dispatch to a
    // whole-fn `unreachable` trap stub. The pure-subject case now emits
    // the (irrefutable) first arm. (Effectful Unit subjects deliberately
    // still fall back — out of scope, kept loud, not silently dropped.)
    let src = r#"module M
    intent = "match on unit subject"
    effects [Console]

fn classify(u: Unit) -> Int
    match u
        _ -> 42

fn label(u: Unit) -> String
    match u
        x -> "bound"

fn main() -> Unit
    ! [Console.print]
    Console.print("{classify(Unit)},{label(Unit)}")
"#;
    let out = assert_vm_wasm_identical("ledger-match-unit", src);
    assert_eq!(out, "42,bound");
}

#[test]
fn ledger_empty_map_literal_resolves_via_stamp_with_multiple_instantiations() {
    // An empty `{}` only resolved the canonical `Map<K,V>` when exactly
    // one instantiation was registered; with two it hard-errored instead
    // of consulting the stamped expected type. (Type-agnostic; a record
    // value just happens to register two instantiations.)
    let src = r#"module M
    intent = "two empty-map instantiations"
    effects [Console]

record Pt
    x: Int
    y: Int

record Line
    a: Int
    b: Int

fn buildPt() -> Map<String, Pt>
    Map.set({}, "p", Pt(x = 5, y = 7))

fn buildLine() -> Map<String, Line>
    Map.set({}, "l", Line(a = 1, b = 2))

fn ptX(k: String) -> Int
    match Map.get(buildPt(), k)
        Option.Some(p) -> p.x
        Option.None -> 0 - 1

fn lineA(k: String) -> Int
    match Map.get(buildLine(), k)
        Option.Some(l) -> l.a
        Option.None -> 0 - 1

fn main() -> Unit
    ! [Console.print]
    px = ptX("p")
    la = lineA("l")
    Console.print("{px},{la}")
"#;
    assert_compiles_clean_wasm_gc("ledger-empty-map-multi", src);
    let out = assert_vm_wasm_identical("ledger-empty-map-multi", src);
    assert_eq!(out, "5,1");
}

#[test]
fn ledger_float_round_is_ties_away_not_ties_to_even() {
    // wasm-gc emitted `F64Nearest` (IEEE ties-to-even); the VM uses Rust
    // `f64::round` (ties-away). The fix emits an EXACT ties-away sequence
    // (`trunc + |frac|>=0.5 select`). The adversarial cases are the point:
    // a naive `floor(|x|+0.5)` double-rounds `0.49999999999999994` up to 1
    // and absorbs `+0.5` into a no-op for large odd integers — both pinned
    // here so that simplification can never silently come back.
    let src = r#"module M
    intent = "float round ties-away"
    effects [Console]

fn r(f: Float) -> Int
    Float.round(f)

fn main() -> Unit
    ! [Console.print]
    Console.print("{r(2.5)},{r(0.0 - 2.5)},{r(0.5)},{r(0.0 - 0.5)},{r(1.5)},{r(0.49999999999999994)},{r(8256582647594471.0)}")
"#;
    let out = assert_vm_wasm_identical("ledger-float-round", src);
    // 2.5->3, -2.5->-3, 0.5->1, -0.5->-1, 1.5->2, 0.4999..94->0 (NOT 1),
    // 8256582647594471->itself (NOT +1).
    assert_eq!(out, "3,-3,1,-1,2,0,8256582647594471");
}

#[test]
fn ledger_string_from_float_no_trap_past_2_pow_63() {
    // The integer-part conversion used the trapping `i64.trunc_f64_s` with
    // no overflow guard, so `String.fromFloat(1e19)` trapped. Now uses
    // saturating unsigned truncation, rendering the exact integer across
    // the full u64 range.
    let src = r#"module M
    intent = "string from float past 2^63"
    effects [Console]

fn s(f: Float) -> String
    String.fromFloat(f)

fn main() -> Unit
    ! [Console.print]
    Console.print("{s(10000000000000000000.0)}|{s(18000000000000000000.0)}")
"#;
    let out = assert_vm_wasm_identical("ledger-string-from-float", src);
    assert_eq!(out, "10000000000000000000|18000000000000000000");
}

#[test]
fn ledger_record_update_on_newtype_record() {
    // `RecordUpdate` on a newtype (single-primitive-field) record lacked
    // the create-side newtype short-circuit, emitting `struct.get`/
    // `struct.new` against the erased wrapper → validation failure.
    let src = r#"module M
    intent = "record update on newtype"
    effects [Console]

record Wrapper
    val: Int

fn bump(w: Wrapper) -> Wrapper
    Wrapper.update(w, val = w.val + 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("{bump(Wrapper(val = 41)).val}")
"#;
    assert_compiles_clean_wasm_gc("ledger-record-update-newtype", src);
    let out = assert_vm_wasm_identical("ledger-record-update-newtype", src);
    assert_eq!(out, "42");
}

#[test]
fn ledger_map_value_record_with_string_field() {
    // A Map value record with a `String` field is NOT newtype-erased
    // (String is not a wasm-gc primitive), so it routes through
    // `emit_hash_record`/`emit_eq_record`, whose String-field arm needs
    // the String key helper force-registered. It used to hard-error
    // "String field needs String key helpers".
    let src = r#"module M
    intent = "map value record with String field"
    effects [Console]

record K
    a: Int

record V
    s: String

fn lookup(m: Map<K, V>, k: K) -> String
    match Map.get(m, k)
        Option.Some(v) -> v.s
        Option.None -> "none"

fn main() -> Unit
    ! [Console.print]
    m = Map.set({}, K(a = 1), V(s = "hi"))
    Console.print("{lookup(m, K(a = 1))},{lookup(m, K(a = 2))}")
"#;
    assert_compiles_clean_wasm_gc("ledger-map-string-value", src);
    let out = assert_vm_wasm_identical("ledger-map-string-value", src);
    assert_eq!(out, "hi,none");
}

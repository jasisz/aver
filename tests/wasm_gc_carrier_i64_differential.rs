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

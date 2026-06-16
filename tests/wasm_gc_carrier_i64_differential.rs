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

/// VM == wasm-gc (carrier-`i64` ON). Divergence ⇒ a carrier value wrapped or
/// a boundary bridge dropped precision.
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
